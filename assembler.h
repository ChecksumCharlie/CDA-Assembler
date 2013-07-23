#ifndef __ASSEMBER_H__
#define __ASSEMBER_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "utility.h"
#include "vector.h"

/*******************************************************************************
 *
 * Invokes the assembling of a collection of MIPS assembly instructions.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  mips_assembly       Vector holding the MIPS assembly instructions which
 *          are to be assembled.
 *  machine_code        Empty vector to hold the 32-bit binary instructions
 *          which will result from the assembling.
 *
 ******************************************************************************/
void
assemble(
         vector_string_t * mips_assembly,
         vector_uint_t * machine_code
        );

/*******************************************************************************
 ***** DEFINE YOUR FUNCTIONS BELOW *********************************************
 ******************************************************************************/

#include <limits.h>
#include <stdint.h>

typedef struct instruction_entry_t instruction_entry_t;
typedef struct label_list_t label_list_t;
typedef uint32_t (*parse_func_t)(instruction_entry_t* me, char* str);

uint32_t 
emit_rtype(
			uint8_t opcode, 
			uint8_t sreg, 
			uint8_t treg, 
			uint8_t dreg, 
			uint8_t shift, 
			uint8_t func
			);

uint32_t 
emit_itype(
			uint8_t opcode,
			uint8_t sreg,
			uint8_t treg, 
			int16_t immi
			);

uint32_t 
emit_jtype(
			uint8_t opcode,
			uint32_t target
			);

void
parse_instruction(
					char* str
				);

instruction_entry_t*
lookup_instruction(
					char* str
					);




label_list_t*
label_list_allocate();

void
label_list_append(
					label_list_t* c
				);


typedef enum instruction_type_t
{
	I_TYPE, J_TYPE, R_TYPE
} instruction_type_t;

typedef enum instruction_opcode_t
{
	ADD, DIV, JR, MFLO, MULT, SLL, SUB, ADDI, BLTZ, BNE, LW, SW, J, JAL
} instruction_opcode_t;

typedef enum register_type_t
{
	ZERO, AT, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, S0, S1, S2, S3, S4, S5, S6, S7, T8, T9, K0, K1, GP, SP, FP, RA
} register_type_t;

typedef struct instruction_entry_t
{
	const char* name;
	const instruction_type_t type;
	const instruction_opcode_t opcode;
	const uint32_t base;
} instruction_entry_t;

typedef struct register_entry_t
{
	const char* name;
	const register_type_t regtype;
} register_entry_t;

typedef struct label_list_t
{
	char* label;
	uint32_t location;
	label_list_t* next;
} label_list_t;

extern instruction_entry_t instruction_table[];
extern register_entry_t register_table[];
static label_list_t* root_label_list;
static vector_uint_t * global_machine_code;

typedef struct tHashNode tHashNode;
typedef struct tHashTable tHashTable;

typedef uint32_t (*pHashFunc)(const tHashTable*, const uint32_t hash);
typedef void (*pHashRebuildCallback)(tHashTable* tb, const uint32_t oldSize, const uint32_t newSize);
typedef void (*pCompFunc)(const void* lhs, const void* rhs);

typedef tHashNode* pHashNode;
typedef tHashTable* pHashTable;

typedef enum
{
	HASH_STRING,
	HASH_WORD,
} eHashType;

typedef struct tHashNode
{
	uint32_t hash;
	union 
	{
		uint32_t uint32;
		uint16_t uint16;
		uint8_t uint8;
		void* ptr;
	} data;
	tHashNode* next;

	union
	{
		size_t word;
		char str;
	} key;

} tHashNode;

typedef struct tHashTable
{
	uint32_t shift;
	uint32_t mask;

	eHashType type;

	uint32_t nbuckets;
	uint32_t nnodes;
	uint32_t rebuildSize;

	pHashFunc hash;
	pCompFunc comp;

	pHashRebuildCallback postRebuild;

	tHashNode** buckets;
} tHashTable;


tHashTable* HashTableAllocate(eHashType type);
tHashNode* HashTableInsertString(tHashTable* tb, const char* str, int* isnew);
tHashNode* HashTableInsertWord(tHashTable* tb, const size_t word, int* isnew);

void HashTableFree(tHashTable* tb);
void HashTableRemove(tHashTable* tb, tHashNode* node);

static tHashNode* HashNodeAllocateString(const tHashTable* table, const char* str);
static tHashNode* HashNodeAllocateWord(const tHashTable* table, const size_t word);

static void HashTableRebuild(tHashTable* tb);

static uint32_t HashString(const char* str);
static uint32_t HashWord(const size_t word); 

static uint32_t FNV1a_Prime_32;
static uint32_t FNV1a_Offset_32;

typedef struct tStringTable tStringTable;
typedef tStringTable* pStringTable;

typedef struct tStringTable
{
	tHashTable* hashes;
	tHashTable* lookup;
	uint32_t _nextId;
} tStringTable;


tStringTable* StringTableAllocate();
void StringTableFree(tStringTable* tb);

const char* StringTableGetString(tStringTable* tb, const uint32_t index);
uint32_t StringTableGetIndex(tStringTable* st, const char* string);
void StringTableSerialize(tStringTable* st, FILE* fh);
tStringTable* StringTableDeserialize(FILE* fh);
const uint32_t StringTableNumStrings(tStringTable* s);
static pStringTable StringTable;
static pHashTable RelocTable;
static pHashTable JmpTable;

#endif // __ASSEMBER_H__
