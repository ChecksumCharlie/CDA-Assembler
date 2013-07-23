/*******************************************************************************
 *******************************************************************************
 *******************************************************************************
 *
 * Do NOT modify this file!!!
 *
 *******************************************************************************
 *
 * This file contains utility functions which may be used in this project.
 *
 *******************************************************************************
 *******************************************************************************
 ******************************************************************************/

#ifndef __UTILITY_H__
#define __UTILITY_H__

#include "vector.h"

#define BUFFER_LENGTH 1024

/*******************************************************************************
 *
 * Reads the contents of a text file, storing the contents to a string vector.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  filename        Name of the file.
 *  vector          String vector to hold the file contents.
 *
 ******************************************************************************/
void
read_file(
          char * filename,
          vector_string_t * vector
         );

/*******************************************************************************
 *
 * Trims away leading and trailing whitespace characters from a string.
 * NOTE: This function modifies the provided string!!!
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  string          String to trim.
 *
 *******************************************************************************
 *
 * RETURNS
 *  A pointer to the provided string.
 *
 ******************************************************************************/
char *
string_trim(char * string);

/*******************************************************************************
 *
 * Writes the contents of an unsigned integer vector to a text file.
 *
 *******************************************************************************
 *
 * PARAMETERS
 *  filename        Name of the file.
 *  vector          Unsigned integer vector to write.
 *
 ******************************************************************************/
void
write_file(
           char * filename,
           vector_uint_t * vector
          );

#endif //__UTILITY_H__
