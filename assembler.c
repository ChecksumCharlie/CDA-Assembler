
#include "assembler.h"

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
        )
{
	int i;
	global_machine_code = machine_code;
	JmpTable = HashTableAllocate(HASH_WORD);
	StringTable = StringTableAllocate();
	RelocTable = HashTableAllocate(HASH_WORD);
	instruction_table = HashTableAllocate(HASH_STRING);
	register_table = HashTableAllocate(HASH_STRING);
	
	{
		register_entry_t* iter;
		for(iter = register_build_table; iter->name != NULL; iter++)
			HashTableInsertString(register_table, iter->name, &i)->data.ptr = iter;
	}

	{
		instruction_entry_t* iter;
		for(iter = instruction_build_table; iter->name != NULL; iter++)
			HashTableInsertString(instruction_table, iter->name, &i)->data.ptr = iter;
	}

	for(i = 0; i < mips_assembly->size; i++)
	{
		parse_instruction(vector_string_get(mips_assembly, i));
	}


	for(i = 0; i < RelocTable->nnodes; i++)
	{
		uint32_t br, abs, inst;
		br = HashTableInsertWord(RelocTable, i, NULL)->data.uint32;
		inst = vector_uint_get(machine_code, br);

		if(inst >> 26 == 1 || inst >> 26 == 5)
		{
			abs = HashTableInsertWord(JmpTable, inst & 0x0000FFFF, NULL)->data.uint32;
			inst = emit_itype(inst >> 26, (inst >> 21) & 0x1F, (inst >> 16) & 0x1F, ((int16_t)abs) - ((int16_t)br + 1));
		}
		else
		{
			abs = HashTableInsertWord(JmpTable, inst & 0x03FFFFFF, NULL)->data.uint32;
			inst = emit_jtype(inst >> 26, ((int32_t)abs) - ((int32_t)br + 1));
		}
		vector_uint_set(machine_code, inst, br);
	}

	HashTableFree(JmpTable);
	HashTableFree(RelocTable);
	HashTableFree(instruction_table);
	HashTableFree(register_table);
	StringTableFree(StringTable);
}

/*******************************************************************************
 ***** IMPLEMENT YOUR FUNCTIONS BELOW ******************************************
 ***** REMEMBER TO DEFINE YOUR FUNCTIONS IN assembler.h ************************
 ******************************************************************************/

uint32_t 
emit_itype(
			uint8_t opcode,
			uint8_t sreg,
			uint8_t treg, 
			int16_t immi
			)
{
	return opcode << 26 | sreg << 21 | treg << 16 | (immi & 0xFFFF);
}

uint32_t 
emit_jtype(
			uint8_t opcode,
			uint32_t target
			)
{
	return opcode << 26 | target & 0x03FFFFFF;
}

uint32_t 
emit_rtype(
			uint8_t opcode, 
			uint8_t sreg, 
			uint8_t treg, 
			uint8_t dreg, 
			uint8_t shift, 
			uint8_t func
			)
{
	return opcode << 26 | sreg << 21 | treg << 16 | dreg << 11 | shift << 6 | func;
}

instruction_entry_t*
lookup_instruction(
					char* str
					)
{
	pHashNode node = HashTableInsertString(instruction_table, str, NULL);
	if(node == NULL)
		return NULL;

	return (instruction_entry_t*)node->data.ptr;
}

register_entry_t*
lookup_register(
					char* str
					)
{
	pHashNode node = HashTableInsertString(register_table, str, NULL);
	return (register_entry_t*)node->data.ptr;
}

uint8_t
parse_register(
			char* str
		)
{
	char* skip;
	long value;
	str++; //Skip $
	value = strtol(str, &skip, 0); 
	if(skip == str)
	{
		return lookup_register(str)->regtype;
	}
	return value;
}

int32_t
parse_number(
				char* str
			)
{
	int32_t val;
	val = strtol(str, NULL, 0);
	return val;
}

void
parse_instruction(
					char* str
				)
{
	int n;
	char *mn, *term;
	uint8_t t, d, s, h;
	uint16_t offset;
	uint32_t target;
	instruction_entry_t* inst;
	t = d = s = h = offset = 0;
	mn = term = NULL;
	inst = NULL;

	if(*str == '.')
		return;

	term = strpbrk(str, " :");
	if(term == NULL)
		return;

	if(*term == ':')
	{
		*term = NULL;
		HashTableInsertWord(JmpTable, StringTableGetIndex(StringTable, str), &n)->data.uint32 = global_machine_code->size;
		return;
	}

	*term = NULL;
	inst = lookup_instruction(str);
	
	if(inst == NULL)
	{
		*term = ' ';
		printf("Unknown instruction '%s'\n", str);
		exit(0);
	}

	str = str + (term - str) + 1;
	term = strtok(str, " ,");
	switch(inst->opcode)
	{
	case SUB:
	case ADD:
			d = parse_register(term);
			term = strtok(NULL, " ,");
			s = parse_register(term);
			term = strtok(NULL, " ");
			t = parse_register(term);
		break;
	case MULT:
	case DIV:
			s = parse_register(term);
			term = strtok(NULL, " ,");
			t = parse_register(term);
		break;
	case JR:
			s = parse_register(term);
		break;
	case MFLO:
			d = parse_register(term);
		break;
	case SLL:
			d = parse_register(term);
			term = strtok(NULL, " ,");
			t = parse_register(term);
			term = strtok(NULL, " ");
			h = parse_number(term);
		break;
	case ADDI:
			t = parse_register(term);
			term = strtok(NULL, " ,");
			s = parse_register(term);
			term = strtok(NULL, " ");
			offset = parse_number(term);
		break;
	case BLTZ:
			s = parse_register(term);
			term = strtok(NULL, " ,");
			offset = StringTableGetIndex(StringTable, term);
			HashTableInsertWord(RelocTable, RelocTable->nnodes, &n)->data.uint32 = global_machine_code->size;
		break;
	case BNE:
			s = parse_register(term);
			term = strtok(NULL, " ,");
			t = parse_register(term);
			term = strtok(NULL, " ,");
			offset = StringTableGetIndex(StringTable, term);
			HashTableInsertWord(RelocTable, RelocTable->nnodes, &n)->data.uint32 = global_machine_code->size;
		break;
	case LW:
	case SW:
			t = parse_register(term);
			term = strtok(NULL, " (");
			offset = parse_number(term);
			term = strtok(NULL, "( )");
			s = parse_register(term);
		break;
	case JAL:
	case J:
		target = StringTableGetIndex(StringTable, term);
		HashTableInsertWord(RelocTable, RelocTable->nnodes, &n)->data.uint32 = global_machine_code->size;
		break;
	case LI:
		{
			int32_t immi;
			t = parse_register(term);
			term = strtok(NULL, " ,");
			immi = parse_number(term);
			offset = (immi >> 16) & 0x0000FFFF;
			vector_uint_add_last(global_machine_code, emit_itype(15, 0, t, offset));
			offset = immi & 0x0000FFFF;
			vector_uint_add_last(global_machine_code, emit_itype(13, t, t, offset));
		}
		return;
	default:
		break;
	}

	switch(inst->type)
	{
	case R_TYPE:
		vector_uint_add_last(global_machine_code, inst->base | emit_rtype(0, s, t, d, h, 0));
		break;
	case I_TYPE:
		vector_uint_add_last(global_machine_code, inst->base | emit_itype(0, s, t, offset));
		break;
	case J_TYPE:
		vector_uint_add_last(global_machine_code, inst->base | emit_jtype(0, target));
		break;
	}
}

instruction_entry_t instruction_build_table[] = 
{
	{"add", R_TYPE, ADD, 32},
	{"div", R_TYPE, DIV, 26},
	{"jr", R_TYPE, JR, 8},
	{"mflo", R_TYPE, MFLO, 18},
	{"mult", R_TYPE, MULT, 8 + 16},
	{"sll", R_TYPE, SLL,  0},
	{"sub", R_TYPE, SUB, 34},
	{"addi", I_TYPE, ADDI, 8 << 26},
	{"bltz", I_TYPE, BLTZ, 1 << 26},
	{"bne", I_TYPE, BNE, 5 << 26},
	{"lw", I_TYPE, LW, 35 << 26},
	{"sw", I_TYPE, SW, 43 << 26},
	{"j", J_TYPE, J, 2 << 26},
	{"jal", J_TYPE, JAL, 3 << 26},
	{"li", I_TYPE, LI, 0},
	NULL
};

register_entry_t register_build_table[] = 
{
	{"zero", ZERO},
	{"at", AT},
	{"v0", V0},
	{"v1", V1},
	{"a0", A0},
	{"a1", A1},
	{"a2", A2},
	{"a3", A3},
	{"t0", T0},
	{"t1", T1},
	{"t2", T2},
	{"t3", T3},
	{"t4", T4},
	{"t5", T5},
	{"t6", T6},
	{"t7", T7},
	{"s0", S0},
	{"s1", S1},
	{"s2", S2},
	{"s3", S3},
	{"s4", S4},
	{"s5", S5},
	{"s6", S6},
	{"s7", S7},
	{"t8", T8},
	{"t9", T9},
	{"k0", K0},
	{"k1", K1},
	{"gp", GP},
	{"sp", SP},
	{"fp", FP},
	{"ra", RA},
	NULL
};

static vector_uint_t * global_machine_code = NULL;
static pStringTable StringTable = NULL;
static pHashTable RelocTable = NULL;
static pHashTable JmpTable = NULL;
static pHashTable register_table = NULL;
static pHashTable instruction_table = NULL;
static uint32_t FNV1a_Offset_32 = 2166136261;
static uint32_t FNV1a_Prime_32 = 16777619;

static uint32_t HashString(const char* str)
{
	size_t strln;
	uint32_t hash;
	for(strln = strlen(str), hash = FNV1a_Offset_32; strln > 0; strln--, str++)
	{
		hash ^= *str;
		hash *= FNV1a_Prime_32;
	}
	return hash;
}

static uint32_t HashWord(const size_t word)
{
	uint32_t hash = FNV1a_Offset_32;
	if(sizeof(size_t) == 4)
	{
		hash ^= (((0xFF << 24) & word) >> 24);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 16) & word) >> 16);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 8) & word) >> 8);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 0) & word) >> 0);
		hash *= FNV1a_Prime_32;
	}
	else if(sizeof(size_t) == 8)
	{
		hash ^= (((0xFF << 56) & word) >> 56);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 48) & word) >> 48);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 40) & word) >> 40);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 32) & word) >> 32);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 24) & word) >> 24);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 16) & word) >> 16);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 8) & word) >> 8);
		hash *= FNV1a_Prime_32;

		hash ^= (((0xFF << 0) & word) >> 0);
		hash *= FNV1a_Prime_32;
	}
	return hash;
}

static uint32_t HashToIndexNormal(const tHashTable* tb, const uint32_t hash)
{
	return ((hash >> tb->shift) ^ (hash & tb->mask)); 
}

static uint32_t HashToIndexTiny(const tHashTable* tb, const uint32_t hash)
{
	return ((hash >> tb->shift) ^ hash) & tb->mask;
}

tHashTable* HashTableAllocate(eHashType hashtype)
{
	tHashTable* tbl;
	tbl = (tHashTable*)malloc(sizeof(tHashTable));
	tbl->nbuckets = 8;
	tbl->nnodes = tbl->mask = 0;
	tbl->rebuildSize = tbl->nbuckets * 3;
	tbl->type = hashtype;

	{ /* Credit to  http://www.hackersdelight.org/hdcodetxt/nlz.c.txt */
		uint32_t h = 0;
		uint32_t k = tbl->nbuckets;
		if (k == 0) h = 32;
		else
		{
			if (k <= 0x0000FFFF) { h = h + 16; k = k << 16; }
			if (k <= 0x00FFFFFF) { h = h + 8; k = k << 8; }
			if (k <= 0x0FFFFFFF) { h = h + 4; k = k << 4; }
			if (k <= 0x3FFFFFFF) { h = h + 2; k = k << 2; }
			if (k <= 0x7FFFFFFF) { h = h + 1; }
		}
		h = 32 - h;
		tbl->shift = h--;
		while(h--)
			tbl->mask = (tbl->mask << 1) + 1;
	}

	if(tbl->shift < 16)
		tbl->hash = HashToIndexTiny;
	else
		tbl->hash = HashToIndexNormal;

	tbl->buckets = (tHashNode**)calloc(tbl->nbuckets, sizeof(tHashNode*));
	return tbl;
}

static tHashNode* HashNodeAllocateString(const tHashTable* table, const char* str)
{
	size_t size;
	tHashNode* node;
	size = sizeof(tHashNode) + (strlen(str) + 1) - sizeof(node->key);
	node = (tHashNode*)malloc(size);
	node->data.ptr = 0;
	strcpy(&(node->key.str), str); 
	return node;
}

static tHashNode* HashNodeAllocateWord(const tHashTable* table, const uint32_t word)
{
	tHashNode* node = (tHashNode*)malloc(sizeof(tHashNode));
	node->data.ptr = 0;
	node->key.word = word;
	return node;
}

tHashNode* HashTableInsertString(tHashTable* tb, const char* str, int* isnew)
{
	uint32_t hash, index;
	tHashNode* node;
	if(tb->type != HASH_STRING)
		return NULL;

	hash = HashString(str);
	index = tb->hash(tb, hash);

	for(node = tb->buckets[index]; node != NULL; node = node->next)
	{
		if(hash != node->hash)
			continue;

		if(strcmp(str, &(node->key.str)) == 0)
		{
			if(isnew)
				*isnew = 0;
			return node;
		}
	}

	if(!isnew)
		return NULL;

	tb->nnodes++;
	if(tb->nnodes >= tb->rebuildSize)
		HashTableRebuild(tb);
	
	*isnew = 1;
	node = HashNodeAllocateString(tb, str);
	node->hash = hash;
	node->next = tb->buckets[index];
	tb->buckets[index] = node;

	return node;
}

tHashNode* HashTableInsertWord(tHashTable* tb, const size_t word, int * isnew)
{
	uint32_t hash, index;
	tHashNode* node;
	if(tb->type  != HASH_WORD)
		return NULL;

	hash = HashWord(word);
	index = tb->hash(tb, hash);

	for(node = tb->buckets[index]; node != NULL; node = node->next)
	{
		if(hash != node->hash)
			continue;

		if(word == node->key.word)
		{
			if(isnew)
				*isnew = 0;

			return node;
		}
	}

	if(!isnew)
		return NULL;

	tb->nnodes++;
	if(tb->nnodes >= tb->rebuildSize)
	{
		HashTableRebuild(tb);
		index = tb->hash(tb, hash);
	}
	
	*isnew = 1;
	node = HashNodeAllocateWord(tb, word);
	node->hash = hash;
	node->next = tb->buckets[index];
	tb->buckets[index] = node;

	return node;
}

static void HashTableRebuild(tHashTable* tb)
{
	uint32_t index, osize;
	tHashNode *node;
	tHashNode **buckets, **oldchain;
	osize = tb->nbuckets;
	buckets = tb->buckets;

	tb->nbuckets *= 4;
	tb->rebuildSize *= 4;
	tb->shift += 2;
	tb->mask = (tb->mask << 2) | 0x3;

	if(tb->shift < 16)
		tb->hash = HashToIndexTiny;
	else
		tb->hash = HashToIndexNormal;

	tb->buckets = (tHashNode**)calloc(tb->nbuckets, sizeof(tHashNode*));

	for(oldchain = buckets; osize > 0; osize--, oldchain++)
	{
		for(node = *oldchain; node != NULL; node = *oldchain)
		{
			*oldchain = node->next;
			index = tb->hash(tb, node->hash);

			node->next = tb->buckets[index];
			tb->buckets[index] = node;
		}

	}


	free(buckets);
}

void HashTableFree(tHashTable* tb)
{
	tHashNode **buckets, *n;
	buckets = tb->buckets;
	for(buckets = tb->buckets; tb->nbuckets > 0; tb->nbuckets--, buckets++)
	{
		for(n = *buckets; n != NULL; n = *buckets)
		{
			*buckets = n->next;
			free(n);
		}
	}
	free(tb->buckets);
	free(tb);
}

tStringTable* StringTableAllocate()
{
	tStringTable* tb;
	int isnew;
	tb = (tStringTable*)malloc(sizeof(tStringTable));
	tb->hashes = HashTableAllocate(HASH_STRING);
	tb->lookup = HashTableAllocate(HASH_WORD);
	tb->_nextId = 0;
	return tb;
}

void StringTableFree(tStringTable* tb)
{
	HashTableFree(tb->lookup);
	HashTableFree(tb->hashes);
	free(tb);
}

const char* StringTableGetString(tStringTable* tb, const uint32_t index)
{
	tHashNode* t = HashTableInsertWord(tb->lookup, index, NULL);
	if(t != NULL)
		return &(((tHashNode*)t->data.ptr)->key.str);
	return NULL;
}

uint32_t StringTableGetIndex(tStringTable* tb, const char* string)
{
	int isnew;
	tHashNode* t;
	t = HashTableInsertString(tb->hashes, string, &isnew);
	if(!isnew)
		return t->data.uint32;

	t->data.uint32 = tb->_nextId;
	HashTableInsertWord(tb->lookup, tb->_nextId, &isnew)->data.ptr = t;
	++(tb->_nextId);
	return t->data.uint32;
}