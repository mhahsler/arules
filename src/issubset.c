/*
C implementation of sparse matrix subset
Author: Ian Johnson
*/

#include <R.h>
#include <Rdefines.h>


void populateMatches(int* matches_for_y, int* x_i, int* x_p, int* y_p, int* y_i, int y_index, int num_rows){

    int y_start_index = x_p[y_index], y_end_index = x_p[y_index+1];

    int num_matches = 0;

    for(int x_index = 0; x_index < num_rows; x_index++){

       int loc = y_p[x_index], end_loc = y_p[x_index+1], curr_col;

       curr_col = y_start_index;

       while(loc < end_loc){

         if (y_i[loc] == x_i[curr_col]) curr_col++;
         if(curr_col == y_end_index) break;

         loc++;

       }


       if(curr_col == y_end_index){
            matches_for_y[num_matches++] = x_index;
       }

    }

    matches_for_y[num_matches] = -1;

}

int copyMatches(int* y_matches, int** output_i, int* output_i_length, int* output_i_last){

  int index = 0;

  while(y_matches[index] != -1){

    if(*output_i_last == *output_i_length - 1){
      int* tmp = malloc(2*(*output_i_length) * sizeof(int));
      memcpy(tmp, *output_i, *output_i_length*sizeof(int));
      *output_i_length *= 2;
      free(*output_i);
      *output_i = tmp;
    }

    (*output_i)[++(*output_i_last)] = y_matches[index++];

  }

  return index;

}


SEXP is_subset(SEXP X_P, SEXP X_I, SEXP X_DIM, SEXP Y_P, SEXP Y_I, SEXP Y_DIM, SEXP OUT_P){

  int* x_p = INTEGER(X_P);
  int* x_i = INTEGER(X_I);

  int* y_p = INTEGER(Y_P);
  int* y_i = INTEGER(Y_I);

  int x_p_length = INTEGER(X_DIM)[1];

  int y_p_length = INTEGER(Y_DIM)[1];
  int y_i_max    = INTEGER(Y_DIM)[0];

  int output_i_length = y_p_length;
  int output_i_last   = -1;
  int* output_i       = malloc((output_i_length+1) * sizeof(int));

  int* output_p = INTEGER(OUT_P);
  int  curr_p   = 0;

  int* y_matches = malloc((output_i_length+1) * sizeof(int));

  //For every item in y, list all matches in x
  for(int y_index = 0; y_index < x_p_length; y_index++){

    populateMatches(y_matches, x_i, x_p, y_p, y_i, y_index, y_p_length);

    curr_p += copyMatches(y_matches, &output_i, &output_i_length, &output_i_last);
    output_p[y_index+1] = curr_p;

  }

  free(y_matches);

  SEXP OUT_I = allocVector(INTSXP, output_i_last+1);
	for(int i = 0; i < output_i_last+1; i++){
		INTEGER(OUT_I)[i] = output_i[i];
	}

  free(output_i);

  return OUT_I;

}
