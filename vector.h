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

#ifndef __VECTOR_H__
#define __VECTOR_H__

#define VECTOR_INIT_CAPACITY 4

/*******************************************************************************
 *
 * Definition of a string vector.
 *
 *******************************************************************************
 *
 * COMPONENTS
 *  capacity            Maximum number of elements that may currently be stored
 *          in the vector.
 *  elements            Array to store the contents of this vector.
 *  size                Number of elements currently stored in the vector.
 *
 ******************************************************************************/
typedef
        struct vector_string
        {
            int capacity;
            char ** elements;
            int size;
        }
    vector_string_t;

/*******************************************************************************
 *
 * Definition of an unsigned integer vector.
 *
 *******************************************************************************
 *
 * COMPONENTS
 *  capacity            Maximum number of elements that may currently be stored
 *          in the vector.
 *  elements            Array to store the contents of this vector.
 *  size                Number of elements currently stored in the vector.
 *
 ******************************************************************************/
typedef
        struct vector_uint
        {
            int capacity;
            unsigned int * elements;
            int size;
        }
    vector_uint_t;

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
vector_string_create();

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
vector_string_destroy(vector_string_t * vector);

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
                 );

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
                       );

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
                      );

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
                    );

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
vector_string_remove_first(vector_string_t * vector);

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
vector_string_remove_last(vector_string_t * vector);

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
                 );

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
                 );

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
                   );

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
vector_string_size(vector_string_t * vector);

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
vector_string_is_empty(vector_string_t * vector);

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
vector_uint_create();

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
vector_uint_destroy(vector_uint_t * vector);

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
               );

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
                     );

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
                    );

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
                  );

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
vector_uint_remove_first(vector_uint_t * vector);

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
vector_uint_remove_last(vector_uint_t * vector);

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
               );

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
               );

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
                 );

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
vector_uint_size(vector_uint_t * vector);

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
vector_uint_is_empty(vector_uint_t * vector);

#endif //__VECTOR_H__
