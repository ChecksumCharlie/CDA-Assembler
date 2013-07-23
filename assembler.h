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
typedef struct register_entry_t register_entry_t;
typedef struct tHashNode tHashNode;
typedef struct tHashTable tHashTable;
typedef struct tStringTable tStringTable;

typedef tHashNode* pHashNode;
typedef tHashTable* pHashTable;
typedef tStringTable* pStringTable;


typedef uint32_t (*pHashFunc)(const tHashTable*, const uint32_t hash);

/*******************************************************************************
*
* Formats the parameters into a R-Type instruction
*
*******************************************************************************
*
* PARAMETERS
*  opcode	Opcode to emit
*
*  sreg		Value to use as the S-Register
*
*  treg		Value to use as the T-Register
*
*  dreg		Value to use as the D-Register
*
*  shift	Value to use as the shift
*
*  func		Value to use as the func
*
* RETURNS
*	32bit instruction containing the given values
*
******************************************************************************/
uint32_t 
	emit_rtype(
	uint8_t opcode, 
	uint8_t sreg, 
	uint8_t treg, 
	uint8_t dreg, 
	uint8_t shift, 
	uint8_t func
	);

/*******************************************************************************
*
* Formats the parameters into a I-Type instruction
*
*******************************************************************************
*
* PARAMETERS
*  opcode	Opcode to emit
*
*  sreg		Value to use as the S-Register
*
*  treg		Value to use as the T-Register
*
*  immi		Value to use as the 16-bit immidiate
*
* RETURNS
*	32bit instruction containing the given values
*
******************************************************************************/
uint32_t 
	emit_itype(
	uint8_t opcode,
	uint8_t sreg,
	uint8_t treg, 
	int16_t immi
	);

/*******************************************************************************
*
* Formats the parameters into a J-Type instruction
*
*******************************************************************************
*
* PARAMETERS
*  opcode	Opcode to emit
*
*  target	Value to use as the 26bit target
*
*
* RETURNS
*	32bit instruction containing the given values
*
******************************************************************************/
uint32_t 
	emit_jtype(
	uint8_t opcode,
	uint32_t target
	);

/*******************************************************************************
*
* Parses the provided string into an instruction or label
*
*******************************************************************************
*
* PARAMETERS
*  str	string to pase
*
*
******************************************************************************/
void
	parse_instruction(
	char* str
	);

/*******************************************************************************
*
* Lookup the instruction represented by 'str'
*
*******************************************************************************
*
* PARAMETERS
*  str	string to parse
*
* RETURNS
*	Pointer to instruction entry representing the register 'str' OR Null if instruction does not exist
******************************************************************************/
instruction_entry_t*
	lookup_instruction(
	char* str
	);

/*******************************************************************************
*
* Lookup the register represented by 'str'
*
*******************************************************************************
*
* PARAMETERS
*  str	string to parse
*
* RETURNS
*	Pointer to register entry representing the register 'str'
******************************************************************************/
register_entry_t*
	lookup_register(
	char* str
	);

/*******************************************************************************
*
* Parses the provided string a register
*
*******************************************************************************
*
* PARAMETERS
*  str	string to parse
*
* RETURNS
*	8bit unsigned value representing the index of register 'str'
******************************************************************************/
uint8_t
	parse_register(
	char* str
	);

/*******************************************************************************
*
* Parses the provided string a number
*
*******************************************************************************
*
* PARAMETERS
*  str	string to parse
*
* RETURNS
*	32bit signed value representing 'str'
******************************************************************************/
int32_t
	parse_number(
	char* str
	);

/*******************************************************************************
 *
 * enum for each type of instruction
 * 
 *******************************************************************************
 *
 * Values
 *  I_TYPE	Instruction of type i
 * 
 *  J_TYPE	Instruction of type j
 *
 *  R_TYPE	Instruction of type r	
 *
 ******************************************************************************/

typedef enum instruction_type_t
{
	I_TYPE, J_TYPE, R_TYPE
} instruction_type_t;

/*******************************************************************************
 *
 * enum for each mips instruction opcode
 *
 *******************************************************************************
 * 
 * Values
 *  ADD		Instruction ADD of MIPS Instruction Set
 * 
 *  DIV		Instruction DIV of MIPS Instruction Set
 * 
 *  JR		Instruction JR of MIPS Instruction Set
 * 
 *  MFLO	Instruction MFLO of MIPS Instruction Set
 * 
 *  MULT	Instruction MULT of MIPS Instruction Set
 * 
 *  SLL		Instruction SLL of MIPS Instruction Set
 * 
 *  SUB		Instruction SUB of MIPS Instruction Set
 * 
 *  ADDI	Instruction ADDI of MIPS Instruction Set
 * 
 *  BLTZ	Instruction BLTZ of MIPS Instruction Set
 * 
 *  BNE		Instruction BNE of MIPS Instruction Set
 * 
 *  LW		Instruction LW of MIPS Instruction Set
 * 
 *  SW		Instruction SW of MIPS Instruction Set
 * 
 *  J		Instruction J of MIPS Instruction Set
 * 
 *  JAL		Instruction JAL of MIPS Instruction Set
 * 
 *  LI		Instruction LI of MIPS Instruction Set
 *
 ******************************************************************************/

typedef enum instruction_opcode_t
{
	ADD, DIV, JR, MFLO, MULT, SLL, SUB, ADDI, BLTZ, BNE, LW, SW, J, JAL, LI
} instruction_opcode_t;

/*******************************************************************************
 *
 * enum for each register in the MIPS machine
 * 
 *******************************************************************************
 *
 * Values
 *  ZERO	Register $0 in the MIPS machine
 *
 *  AT		Register $at in the MIPS machine
 *
 *  V0		Register $v0 in the MIPS machine
 *
 *  V1		Register $v1 in the MIPS machine
 *
 *  A0		Register $a0 in the MIPS machine
 *
 *  A1		Register $a1 in the MIPS machine
 *
 *  A2		Register $a2 in the MIPS machine
 *
 *  A3		Register $a3 in the MIPS machine
 *
 *  T0		Register $t0 in the MIPS machine
 *
 *  T1		Register $t1 in the MIPS machine
 *
 *  T2		Register $t2 in the MIPS machine
 *
 *  T3		Register $t3 in the MIPS machine
 *
 *  T4		Register $t4 in the MIPS machine
 *
 *  T5		Register $t5 in the MIPS machine
 *
 *  T6		Register $t6 in the MIPS machine
 *
 *  T7		Register $t7 in the MIPS machine
 *
 *  S0		Register $s0 in the MIPS machine
 *
 *  S1		Register $s1 in the MIPS machine
 *
 *  S2		Register $s2 in the MIPS machine
 *
 *  S3		Register $s3 in the MIPS machine
 *
 *  S4		Register $s4 in the MIPS machine
 *
 *  S5		Register $s5 in the MIPS machine
 *
 *  S6		Register $s6 in the MIPS machine
 *
 *  S7		Register $s7 in the MIPS machine
 *
 *  T8		Register $t8 in the MIPS machine
 *
 *  T9		Register $t9 in the MIPS machine
 *
 *  K0		Register $k0 in the MIPS machine
 *
 *  K1		Register $k1 in the MIPS machine
 *
 *  GP		Register $gp in the MIPS machine
 *
 *  SP		Register $sp in the MIPS machine
 *
 *  FP		Register $fp in the MIPS machine
 *
 *  RA		Register $ra in the MIPS machine
 *
 ******************************************************************************/

typedef enum register_type_t
{
	ZERO, AT, V0, V1, A0, A1, A2, A3, T0, T1, T2, T3, T4, T5, T6, T7, S0, S1, S2, S3, S4, S5, S6, S7, T8, T9, K0, K1, GP, SP, FP, RA
} register_type_t;

/*******************************************************************************
*
* Instruction Entry structure
*
*******************************************************************************
*
* MEMBERS
* name	Null terminated string representing instruction name
*
* type	instruction_type_t enum representing intruction type {I_TYPE, J_TYPE, R_TYPE}
*
* opcode	instruction_opcode_t enum representing integer opcode of instruction {ADD, DIV, ...}
*
* base	bit-string template for instruction
*
******************************************************************************/
typedef struct instruction_entry_t
{
	const char* name;
	const instruction_type_t type;
	const instruction_opcode_t opcode;
	const uint32_t base;
} instruction_entry_t;

/*******************************************************************************
*
* Register Entry structure
*
*******************************************************************************
*
* MEMBERS
* name	Null terminated string representing register name
*
* regtype	enum representing integer id of register with name 'name'
*
******************************************************************************/
typedef struct register_entry_t
{
	const char* name;
	const register_type_t regtype;
} register_entry_t;

typedef enum
{
	HASH_STRING,
	HASH_WORD,
} eHashType;

/*******************************************************************************
*
* Hash Node structure
*
*******************************************************************************
*
* MEMBERS
* hash	Stored hash of 'key'
*
* data	union of possible storage types
*
* next	Pointer to next node in bucket
*
* key	- Variable Size Data Structure! - word or null-terminated string representing the key for this node
*
******************************************************************************/
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

/*******************************************************************************
*
* Hash Table structure
*
*******************************************************************************
*
* MEMBERS
* shift		Internal field - do not use - #buckets / 2, used in hash calculation
*
* mask		Internal field - do not use - bitmask for indexing
*
* type		Key-Type of HashTable, either HASH_WORD or HASH_STRING
*
* nbuckets	Number of buckets currently allocated by hashtable
*
* nnodes	Number of nodes currently stored within hashtable
*
* rebuildSize	Min number of nodes before rebuilding hashtable
*
* hash		Pointer to hashing reduction function
*
* buckets	Pointer to buckets currently allocated by hashtable
*
******************************************************************************/
typedef struct tHashTable
{
	uint32_t shift;
	uint32_t mask;

	eHashType type;

	uint32_t nbuckets;
	uint32_t nnodes;
	uint32_t rebuildSize;

	pHashFunc hash;

	tHashNode** buckets;
} tHashTable;

/*******************************************************************************
*
* String Table structure
*
*******************************************************************************
*
* MEMBERS
* hashes	String-Key hashtable used to map strings => integers
*
* lookup	Word-Key hashtable used to map integers => strings
*
* _nextId	Internal field - do not use - represents next available index
*
******************************************************************************/
typedef struct tStringTable
{
	tHashTable* hashes;
	tHashTable* lookup;
	uint32_t _nextId;
} tStringTable;

/*******************************************************************************
*
* Allocates a HashTable and binds it to the key type provided, mixing of key-types is not valid
*
*******************************************************************************
*
* PARAMETERS
*  type		eHashTable { HASH_WORD, HASH_STRING } representing the data to store in this hashtable
*
* RETURNS
*	a new hash table
*
******************************************************************************/
tHashTable* HashTableAllocate(eHashType type);

/*******************************************************************************
*
* Inserts and finds a String-Key node into a hashtable
*
*******************************************************************************
*
* PARAMETERS
*  tb		table to lookup or insert the string into
*
*  str		string to use as key
*
*  isnew	pointer to int, set to 1 if string was not present before insert, set to 0 otherwise
*
* RETURNS
*	hash node representing key 'str'
*
******************************************************************************/
tHashNode* HashTableInsertString(tHashTable* tb, const char* str, int* isnew);

/*******************************************************************************
*
* Inserts and finds a Word-Key node into a hashtable
*
*******************************************************************************
*
* PARAMETERS
*  tb		table to lookup or insert the word into
*
*  word		word to use as key
*
*  isnew	pointer to int, set to 1 if word was not present before insert, set to 0 otherwise
*
* RETURNS
*	hash node representing key 'word'
*
******************************************************************************/
tHashNode* HashTableInsertWord(tHashTable* tb, const size_t word, int* isnew);

/*******************************************************************************
*
* Frees a hash table's book keeping information
*
*******************************************************************************
*
* PARAMETERS
*  table	table to free
*
******************************************************************************/
void HashTableFree(tHashTable* tb);

/*******************************************************************************
*
* Allocates a String-Type hash node
*
*******************************************************************************
*
* PARAMETERS
*  table	table to rebuild
*
*  word	word to use as node key
*
* RETURNS
*	new hash node
*
******************************************************************************/
static tHashNode* HashNodeAllocateString(const tHashTable* table, const char* str);

/*******************************************************************************
*
* Allocates a Word-Type hash node
*
*******************************************************************************
*
* PARAMETERS
*  table	table to store the node into
*
*  word	word to use as node key
*
* RETURNS
*	new hash node
******************************************************************************/
static tHashNode* HashNodeAllocateWord(const tHashTable* table, const size_t word);

/*******************************************************************************
*
* Rebuilds a full hashtable
*
*******************************************************************************
*
* PARAMETERS
*  tb	table to rebuild
*
*
******************************************************************************/
static void HashTableRebuild(tHashTable* tb);

/*******************************************************************************
*
* Hashes a string
*
*******************************************************************************
*
* PARAMETERS
*  str	string to hash
*
* RETURNS
*	32bit hash 'str'
*
******************************************************************************/
static uint32_t HashString(const char* str);

/*******************************************************************************
*
* Hashes a system word
*
*******************************************************************************
*
* PARAMETERS
*  word       value to hash
*
* RETURNS
*	32bit hash of 'word'
*
******************************************************************************/
static uint32_t HashWord(const size_t word); 

/*******************************************************************************
*
* Allocates the resources associated with the book keeping information of a string table
*
*******************************************************************************
*
* RETURNS
*  A new string table
*
******************************************************************************/
tStringTable* StringTableAllocate();

/*******************************************************************************
*
* Frees the resources associated with the book keeping information of a string table
*
*******************************************************************************
*
* PARAMETERS
*  tb       String table to free
*
******************************************************************************/
void StringTableFree(tStringTable* tb);

/*******************************************************************************
*
* Returns the string associated with 'index' from string table 'tb'
*
*******************************************************************************
*
* PARAMETERS
*  tb       String table that contains the string to lookup
*
*  index	index to lookup in the string table
*
* RETURNS
*	string associated with index 'index'
*
******************************************************************************/
const char* StringTableGetString(tStringTable* tb, const uint32_t index);

/*******************************************************************************
*
* Returns the index of string 'string' in string table 'st'
*
*******************************************************************************
*
* PARAMETERS
*  st       String table that contains the string to lookup
*
*  string	String to look up in the string table
*
* RETURNS
*	index associated with string 'string'
******************************************************************************/
uint32_t StringTableGetIndex(tStringTable* st, const char* string);

static uint32_t FNV1a_Prime_32;
static uint32_t FNV1a_Offset_32;
static pStringTable StringTable;
static pHashTable RelocTable;
static pHashTable JmpTable;
static pHashTable register_table;
static pHashTable instruction_table;
static vector_uint_t * global_machine_code;

extern instruction_entry_t instruction_build_table[];
extern register_entry_t register_build_table[];


#endif // __ASSEMBER_H__
