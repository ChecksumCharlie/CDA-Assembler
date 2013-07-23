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

#include "assembler.h"

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
         )
{
    char buffer[BUFFER_LENGTH];
    FILE * fp;
    
    // attempt to open the file
    if (!(fp = fopen(filename, "r")))
    {
        fprintf(stderr, "*** failed to open the file %s ***\n", filename);
        exit(1);
    }
    
    // read the file
    while (fgets(buffer, BUFFER_LENGTH - 1, fp))
    {
        if (strlen(string_trim(buffer)) > 0)
        {
            vector_string_add_last(vector, buffer);
        }
    }
    
    // close the file
    fclose(fp);
}

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
          )
{
    FILE * fp;
    int i;
    
    // attempt to open the file
    if (!(fp = fopen(filename, "w")))
    {
        fprintf(stderr, "*** failed to open the file %s ***\n", filename);
        exit(1);
    }
    
    // write the file
    for (i = 0; i < vector->size; i++)
    {
        fprintf(fp, "%u\n", vector_uint_get(vector, i));
    }
    
    // close the file
    fclose(fp);
}

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
string_trim(char * string)
{
    int i, j, k, l;
    
    // find out how many leading whitespace characters exist
    i = 0;
    while (string[i] <= ' ' && string[i] != '\0')
    {
        i++;
    }
    
    // find out how many trailing whitespace characters exist
    j = strlen(string) - 1;
    while (0 < j && string[j] <= ' ')
    {
        j--;
    }
    
    // shift characters down to trim
    for (k = 0, l = i; l <= j; k++, l++)
    {
        string[k] = string[l];
    }
    string[k] = '\0';
    
    return string;
}
