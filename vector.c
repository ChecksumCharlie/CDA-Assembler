/*******************************************************************************
 *******************************************************************************
 *******************************************************************************
 *
 * Do NOT modify this file!!!
 *
 *******************************************************************************
 *
 * This file contains implementations of two vector data structures: one for
 * strings and one for unsigned integers.
 *
 * A vector is a simple, dynamically-sized array, i.e. a light-weight
 * arraylist.
 *
 *******************************************************************************
 *******************************************************************************
 ******************************************************************************/

#include "assembler.h"

/*******************************************************************************
 *******************************************************************************
 ******************************************************************************/

void
vector_string_change_capacity(
                              vector_string_t * vector,
                              int new_capacity
                             );


void
vector_uint_change_capacity(
                            vector_uint_t * vector,
                            int new_capacity
                           );


/*******************************************************************************
 *******************************************************************************
 ******************************************************************************/

/*******************************************************************************
 *
 * Creates a new vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Pointer to the new vector.
 *
 ******************************************************************************/
vector_string_t *
vector_string_create()
{
    vector_string_t * v;
    int i;
    
    if (!(v = malloc(sizeof(vector_string_t))))
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(stderr, "vector_string_create vector_string_t\n");
        
        exit(1);
    }
    
    v->capacity = VECTOR_INIT_CAPACITY;
    v->elements = malloc(VECTOR_INIT_CAPACITY * sizeof(char *));
    v->size = 0;
    
    if (!v->elements)
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(stderr, "vector_string_create vector->elements\n");
        exit(1);
    }
    
    for (i = 0; i < VECTOR_INIT_CAPACITY; i++)
    {
        if (!(v->elements[i] = malloc(BUFFER_LENGTH * sizeof(char))))
        {
            fprintf(stderr, "*** malloc failed ***\n");
            fprintf(stderr, "vector_string_create vector->elements[i]\n");
            exit(1);
        }
    }
    
    return v;
}

/*******************************************************************************
 *
 * Completely deallocates a vector, purging the contents of the vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer the vector that is to be deallocated.
 *
 ******************************************************************************/
void
vector_string_destroy(vector_string_t * vector)
{
    int i;
    if (vector->elements)
    {
        for (i = 0; i < vector->capacity; i++)
        {
            if (vector->elements[i])
            {
                free(vector->elements[i]);
            }
        }
        free(vector->elements);
    }
    free(vector);
}

/*******************************************************************************
 *
 * Inserts a string into a vector at the specified index. All elements
 * postceding this index will be moved to the right by one.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  string              String to insert.
 *  index               Index at which to insert the string. If this is
 *          negative, no insertion will happen. If this is greater than the
 *          size of the vector, the insertion will occur at the end of the
 *          vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If index is negative, -1.
 *
 ******************************************************************************/
int
vector_string_add(
                  vector_string_t * vector,
                  char * string,
                  int index
                 )
{
    int i;
    
    // impossible index
    if (index < 0)
    {
        return -1;
    }
    
    // make room for the new element - double capacity if need be
    if (vector->size + 1 >= vector->capacity)
    {
        vector_string_change_capacity(vector, vector->capacity << 1);
    }
    
    // need to move elements down by one
    if (index < vector->size)
    {
        for (i = vector->size; index < i; i--)
        {
            strcpy(vector->elements[i], vector->elements[i - 1]);
        }
        strcpy(vector->elements[index], string);
    }
    else
    {
        strcpy(vector->elements[vector->size], string);
    }
    
    return ++vector->size;
}

/*******************************************************************************
 *
 * Convenience function which inserts a string at the very beginning of
 * a vector.
 *
 * This function is the equivalent of calling:
 *      vector_string_add(vector, string, 0)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  string              String to insert.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector.
 *
 ******************************************************************************/
int
vector_string_add_first(
                        vector_string_t * vector,
                        char * string
                       )
{
    return vector_string_add(vector, string, 0);
}

/*******************************************************************************
 *
 * Convenience function which inserts a string at the very end of
 * a vector.
 *
 * This function is the equivalent of calling:
 *      vector_string_add(vector, string, vector_string_size(vector))
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  string              String to insert.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector.
 *
 ******************************************************************************/
int
vector_string_add_last(
                       vector_string_t * vector,
                       char * string
                      )
{
    return vector_string_add(vector, string, vector->size);
}

/*******************************************************************************
 *
 * Removes the string located at the provided index from a vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  index               Index at which to remove the string. If this is
 *          out-of-bounds (index < 0 || index <= vector_string_size(vector)),
 *          no removal will occur.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If index is out-of-bounds, -1.
 *
 ******************************************************************************/
int
vector_string_remove(
                     vector_string_t * vector,
                     int index
                    )
{
    int i;
    
    // impossible index
    if (index < 0 || vector->size <= index)
    {
        return -1;
    }
    
    // remove the element
    if (1 < vector->size && index < vector->size - 1)
    {
        for (i = index; i < vector->size; i++)
        {
            strcpy(vector->elements[i], vector->elements[i + 1]);
        }
    }
    
    // shrink the vector
    vector->size--;
    if (VECTOR_INIT_CAPACITY < vector->capacity
            && vector->size < vector->capacity >> 1)
    {
       vector_string_change_capacity(vector, vector->capacity >> 1);
    }
    
    return vector->size;
}

/*******************************************************************************
 *
 * Convenience function which removes the element at the very beginning
 * of a vector.
 *
 * This function is the equivalent of calling:
 *      vector_string_remove(vector, 0)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Vector which is to have an element removed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If the vector was empty, -1.
 *
 ******************************************************************************/
int
vector_string_remove_first(vector_string_t * vector)
{
    return vector_string_remove(vector, 0);
}

/*******************************************************************************
 *
 * Convenience function which removes the element at the very end
 * of a vector.
 *
 * This function is the equivalent of calling:
 *      vector_string_remove(vector, vector_string_size(vector) - 1)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Vector which is to have an element removed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If the vector was empty, -1.
 *
 ******************************************************************************/
int
vector_string_remove_last(vector_string_t * vector)
{
    return vector_string_remove(vector, vector->size - 1);
}

/*******************************************************************************
 *
 * Replaces the value at the provided index with the provided element.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *  string              Element to set.
 *  index               Index to set. If this index is out-of-bounds
 *             (index < 0 || vector_string_size(vector) <= index), no set will
 *             be performed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  true on success, false on failure.
 *
 ******************************************************************************/
int
vector_string_set(
                  vector_string_t * vector,
                  char * string,
                  int index
                 )
{
    if (index < 0 || vector->size <= index)
    {
        return 0;
    }
    
    strcpy(vector->elements[index], string);
    return vector->size;
}

/*******************************************************************************
 *
 * Retrieves the element at the specified index of the vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector to be searched.
 *  index               Index at which to retrieve. If this index is
 *          out-of-bounds, a NULL will be retrieved.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Element located at the specified index. If the index is out-of-bounds,
 *  NULL.
 *
 ******************************************************************************/
char *
vector_string_get(
                  vector_string_t * vector,
                  int index
                 )
{
    return (0 <= index && index < vector->size)
           ? vector->elements[index]
           : NULL;
}

/*******************************************************************************
 *
 * Convenience function which prints out a vector to the specified file.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *  fp                  Pointer to a file to which the contents of vector will
 *          be printed.
 *
 ******************************************************************************/
void
vector_string_print(
                    vector_string_t * vector,
                    FILE * fp
                   )
{
    int i;
    
    fprintf(fp, "[\n");
    fprintf(fp, "\tcapacity : %i\n", vector->capacity);
    fprintf(fp, "\tsize : %i\n", vector_string_size(vector));
    fprintf(fp, "\telements : {\n");
    for (i = 0; i < vector->size; i++)
    {
        fprintf(fp, "\t%7i) %s\n", i, vector->elements[i]);
    }
    fprintf(fp, "\t}\n");
    fprintf(fp, "]\n");
}

/*******************************************************************************
 *
 * Retrieves the number of elements contained within a vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Number of elements contained within the vector.
 *
 ******************************************************************************/
int
vector_string_size(vector_string_t * vector)
{
    return vector->size;
}

/*******************************************************************************
 *
 * Determines if a vector contains no elements.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  true if the vector is empty and false otherwise.
 *
 ******************************************************************************/
char
vector_string_is_empty(vector_string_t * vector)
{
    return vector->size == 0;
}

/*******************************************************************************
 *
 * Changes the capacity of a provided vector to the provided capacity.
 *
 ******************************************************************************/
void
vector_string_change_capacity(
                              vector_string_t * vector,
                              int new_capacity
                             )
{
    char ** tmp;
    int tmp_size;
    int i;
    
    // preserve the contents of the old elements array
    tmp = malloc(vector->size * sizeof(char *));
    for (i = 0; i < vector->size; i++)
    {
        tmp[i] = malloc(BUFFER_LENGTH * sizeof(char));
        strcpy(tmp[i], vector->elements[i]);
        free(vector->elements[i]);
    }
    free(vector->elements);
    
    // create one with the new capacity
    vector->elements = malloc(new_capacity * sizeof(char *));
    if (!vector->elements)
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(
                stderr,
                "vector_string_change_capacity from %i to %i\n",
                vector->capacity,
                new_capacity
               );
        exit(1);
    }
    for (i = 0; i < new_capacity; i++)
    {
        if (!(vector->elements[i] = malloc(BUFFER_LENGTH * sizeof(char))))
        {
            fprintf(stderr, "*** malloc failed ***\n");
            fprintf(
                    stderr,
                    "vector_string_change_capacity from %i to %i\n",
                    vector->capacity,
                    new_capacity
                   );
            exit(1);
        }
        if (i < vector->size)
        {
            strcpy(vector->elements[i], tmp[i]);
            free(tmp[i]);
        }
    }
    free(tmp);
    vector->capacity = new_capacity;
}

/*******************************************************************************
 *******************************************************************************
 ******************************************************************************/

/*******************************************************************************
 *
 * Creates a new vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Pointer to the new vector.
 *
 ******************************************************************************/
vector_uint_t *
vector_uint_create()
{
    vector_uint_t * v;
    int i;
    
    if (!(v = malloc(sizeof(vector_uint_t))))
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(stderr, "vector_uint_create vector_uint_t\n");
        
        exit(1);
    }
    
    v->capacity = VECTOR_INIT_CAPACITY;
    v->elements = malloc(VECTOR_INIT_CAPACITY * sizeof(unsigned int));
    v->size = 0;
    
    if (!v->elements)
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(stderr, "vector_uint_create vector->elements\n");
        exit(1);
    }
    
    return v;
}

/*******************************************************************************
 *
 * Completely deallocates a vector, purging the contents of the vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer the vector that is to be deallocated.
 *
 ******************************************************************************/
void
vector_uint_destroy(vector_uint_t * vector)
{
    if (vector->elements)
    {
        free(vector->elements);
    }
    free(vector);
}

/*******************************************************************************
 *
 * Inserts an unsigned int into a vector at the specified index. All elements
 * postceding this index will be moved to the right by one.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  nubmer              Value to insert.
 *  index               Index at which to insert the value. If this is
 *          negative, no insertion will happen. If this is greater than the
 *          size of the vector, the insertion will occur at the end of the
 *          vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If index is negative, -1.
 *
 ******************************************************************************/
int
vector_uint_add(
                vector_uint_t * vector,
                unsigned int number,
                int index
               )
{
    int i;
    
    // impossible index
    if (index < 0)
    {
        return -1;
    }
    
    // make room for the new element - double capacity if need be
    if (vector->size + 1 >= vector->capacity)
    {
        vector_uint_change_capacity(vector, vector->capacity << 1);
    }
    
    // need to move elements down by one
    if (index < vector->size)
    {
        for (i = vector->size; index < i; i--)
        {
            vector->elements[i] = vector->elements[i - 1];
        }
        vector->elements[index] = number;
    }
    else
    {
        vector->elements[vector->size] = number;
    }
    
    return ++vector->size;
}

/*******************************************************************************
 *
 * Convenience function which inserts a value at the very beginning of
 * a vector.
 *
 * This function is the equivalent of calling:
 *      vector_uint_add(vector, value, 0)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  number              Number to insert.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector.
 *
 ******************************************************************************/
int
vector_uint_add_first(
                      vector_uint_t * vector,
                      unsigned int number
                     )
{
    return vector_uint_add(vector, number, 0);
}

/*******************************************************************************
 *
 * Convenience function which inserts a value at the very end of
 * a vector.
 *
 * This function is the equivalent of calling:
 *      vector_uint_add(vector, value, vector_uint_size(vector))
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  number              Value to insert.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector.
 *
 ******************************************************************************/
int
vector_uint_add_last(
                     vector_uint_t * vector,
                     unsigned int number
                    )
{
    return vector_uint_add(vector, number, vector->size);
}

/*******************************************************************************
 *
 * Removes the value located at the provided index from a vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector.
 *  index               Index at which to remove the value. If this is
 *          out-of-bounds (index < 0 || index <= vector_uint_size(vector)),
 *          no removal will occur.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If index is out-of-bounds, -1.
 *
 ******************************************************************************/
int
vector_uint_remove(
                   vector_uint_t * vector,
                   int index
                  )
{
    int i;
    
    // impossible index
    if (index < 0 || vector->size <= index)
    {
        return -1;
    }
    
    // remove the element
    if (1 < vector->size && index < vector->size - 1)
    {
        for (i = index; i < vector->size; i++)
        {
            vector->elements[i] = vector->elements[i + 1];
        }
    }
    
    // shrink the vector
    vector->size--;
    if (VECTOR_INIT_CAPACITY < vector->capacity
            && vector->size < vector->capacity >> 1)
    {
       vector_uint_change_capacity(vector, vector->capacity >> 1);
    }
    
    return vector->size;
}

/*******************************************************************************
 *
 * Convenience function which removes the element at the very beginning
 * of a vector.
 *
 * This function is the equivalent of calling:
 *      vector_uint_remove(vector, 0)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Vector which is to have an element removed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If the vector was empty, -1.
 *
 ******************************************************************************/
int
vector_uint_remove_first(vector_uint_t * vector)
{
    return vector_uint_remove(vector, 0);
}

/*******************************************************************************
 *
 * Convenience function which removes the element at the very end
 * of a vector.
 *
 * This function is the equivalent of calling:
 *      vector_uint_remove(vector, vector_uint_size(vector) - 1)
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Vector which is to have an element removed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  New size of the vector. If the vector was empty, -1.
 *
 ******************************************************************************/
int
vector_uint_remove_last(vector_uint_t * vector)
{
    return vector_uint_remove(vector, vector->size - 1);
}

/*******************************************************************************
 *
 * Replaces the value at the provided index with the provided element.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *  number              Element to set.
 *  index               Index to set. If this index is out-of-bounds
 *             (index < 0 || vector_string_size(vector) <= index), no set will
 *             be performed.
 *
 *******************************************************************************
 *
 * RETURNS
 *  true on success, false on failure.
 *
 ******************************************************************************/
int
vector_uint_set(
                vector_uint_t * vector,
                unsigned int number,
                int index
               )
{
    if (index < 0 || vector->size <= index)
    {
        return 0;
    }
    
    vector->elements[index] = number;
    return vector->size;
}

/*******************************************************************************
 *
 * Retrieves the element at the specified index of the vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to the vector to be searched.
 *  index               Index at which to retrieve. If this index is
 *          out-of-bounds, a NULL will be retrieved.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Element located at the specified index. If the index is out-of-bounds,
 *  0.
 *
 ******************************************************************************/
unsigned int
vector_uint_get(
                vector_uint_t * vector,
                int index
               )
{
    return (0 <= index && index < vector->size)
           ? vector->elements[index]
           : 0;
}

/*******************************************************************************
 *
 * Convenience function which prints out a vector to the specified file.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *  fp                  Pointer to a file to which the contents of vector will
 *          be printed.
 *
 ******************************************************************************/
void
vector_uint_print(
                  vector_uint_t * vector,
                  FILE * fp
                 )
{
    int i;
    
    fprintf(fp, "[\n");
    fprintf(fp, "\tcapacity : %i\n", vector->capacity);
    fprintf(fp, "\tsize : %i\n", vector_uint_size(vector));
    fprintf(fp, "\telements : {\n");
    for (i = 0; i < vector->size; i++)
    {
        fprintf(fp, "\t%7i) %u\n", i, vector_uint_get(vector, i));
    }
    fprintf(fp, "\t}\n");
    fprintf(fp, "]\n");
}

/*******************************************************************************
 *
 * Retrieves the number of elements contained within a vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  Number of elements contained within the vector.
 *
 ******************************************************************************/
int
vector_uint_size(vector_uint_t * vector)
{
    return vector->size;
}

/*******************************************************************************
 *
 * Determines if a vector contains no elements.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  vector              Pointer to a vector.
 *
 *******************************************************************************
 *
 * RETURNS
 *  true if the vector is empty and false otherwise.
 *
 ******************************************************************************/
char
vector_uint_is_empty(vector_uint_t * vector)
{
    return vector->size == 0;
}

/*******************************************************************************
 *
 * Changes the capacity of a provided vector to the provided capacity.
 *
 ******************************************************************************/
void
vector_uint_change_capacity(
                            vector_uint_t * vector,
                            int new_capacity
                           )
{
    unsigned int * tmp;
    int tmp_size;
    int i;
    
    // preserve the contents of the old elements array
    tmp = malloc(vector->size * sizeof(unsigned));
    for (i = 0; i < vector->size; i++)
    {
        tmp[i] = vector->elements[i];
    }
    free(vector->elements);
    
    // create one with the new capacity
    vector->elements = malloc(new_capacity * sizeof(unsigned));
    if (!vector->elements)
    {
        fprintf(stderr, "*** malloc failed ***\n");
        fprintf(
                stderr,
                "vector_uint_change_capacity from %i to %i\n",
                vector->capacity,
                new_capacity
               );
        exit(1);
    }
    for (i = 0; i < new_capacity; i++)
    {
        if (i < vector->size)
        {
            vector->elements[i] = tmp[i];
        }
    }
    free(tmp);
    vector->capacity = new_capacity;
}




