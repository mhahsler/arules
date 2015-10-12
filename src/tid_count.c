#include <R.h>
#include <R_ext/Utils.h>
#include <Rdefines.h>

SEXP R_tid_support(SEXP tidLists, SEXP itemsets) {
    /* all are already ngCMatrix */

    int n_of_trans, n_of_items, n_of_sets;
    int *t_i, * t_p, *i_i, *i_p, *buffer;
    int i, j, k;
    int item, count, length_of_itemset;

    SEXP support;

    n_of_trans = INTEGER(GET_SLOT(tidLists, install("Dim")))[0];
    n_of_items = INTEGER(GET_SLOT(tidLists, install("Dim")))[1];
    t_i = INTEGER(GET_SLOT(tidLists, install("i")));
    t_p = INTEGER(GET_SLOT(tidLists, install("p")));
    
    if(INTEGER(GET_SLOT(itemsets, install("Dim")))[0] != n_of_items) 
        error("transactions and itemsets are not compatible");
    
    n_of_sets = INTEGER(GET_SLOT(itemsets, install("Dim")))[1];
    i_i = INTEGER(GET_SLOT(itemsets, install("i")));
    i_p = INTEGER(GET_SLOT(itemsets, install("p")));

    /* result */
    PROTECT(support = allocVector(INTSXP, n_of_sets));
    
    /* count buffer */
    buffer = (int *) R_alloc(n_of_trans, sizeof(int));
    for(i = 0; i < n_of_trans; i++) buffer[i] = 0;

    /* go through itemsets */
    /* Rprintf("doing itemset "); */
    for(i = 0; i < (n_of_sets); i++) {
        /* Rprintf("%d ", i); */
        
        /* go through itemset */
        for(j = i_p[i]; j < i_p[i+1]; j++) {
            item = i_i[j];
            /* Rprintf("\n\titem %d\n", item); */
            
            /* t_p[item]; */
            
            /*  add individual item tidlist */
            for(k = t_p[item]; k < t_p[item+1]; k++){
                buffer[t_i[k]]++; 
            }
        }
        
        /* count support count */
        length_of_itemset = i_p[i+1]-i_p[i];
        count = 0;
        for(j = 0; j < n_of_trans; j++){
            if(buffer[j] == length_of_itemset) count++;

            /* clear buffer again */
            buffer[j] = 0; 
        }
        INTEGER(support)[i] = count;
        /* Rprintf("(%d) ", count); */

        if(i%100 == 0) R_CheckUserInterrupt();
    } 
        
   /* Rprintf("done...\n"); */

    UNPROTECT(1);
    return support;
}

SEXP R_tid_rules(SEXP tidLists, SEXP itemsets) {
    /* all are already ngCMatrix */

    int n_of_trans, n_of_items, n_of_sets;
    int *t_i, * t_p, *i_i, *i_p, *buffer, *hits;
    int i, j, k, l, m, n = 0;
    int lhs_i_counter = 0;
    int item, count, subset_count;
    int length_of_itemset;
    int n_of_rules, length_lhs;
    double dbl_n_of_trans;

    SEXP support, conf, result;
    SEXP lhs_i, lhs_p, rhs_i, rhs_p, Dim;

    n_of_trans = INTEGER(GET_SLOT(tidLists, install("Dim")))[0];
    dbl_n_of_trans = (double) n_of_trans;
    n_of_items = INTEGER(GET_SLOT(tidLists, install("Dim")))[1];
    t_i = INTEGER(GET_SLOT(tidLists, install("i")));
    t_p = INTEGER(GET_SLOT(tidLists, install("p")));
    
    if(INTEGER(GET_SLOT(itemsets, install("Dim")))[0] != n_of_items) 
        error("transactions and itemsets are not compatible");
    
    n_of_sets = INTEGER(GET_SLOT(itemsets, install("Dim")))[1];
    i_i = INTEGER(GET_SLOT(itemsets, install("i")));
    i_p = INTEGER(GET_SLOT(itemsets, install("p")));

    
    /* get length for result */
    n_of_rules = 0;
    length_lhs = 0;
    for(i = 0; i < n_of_sets; i++) {
        j = i_p[i+1] - i_p[i];
        if (j>1) {
            /* drop 1-itemsets */
            n_of_rules += j;   
            length_lhs += j*(j-1); 
        }
    }
    
    
    /* results */
    PROTECT(result = allocVector(VECSXP, 7));
    
    PROTECT(support = allocVector(REALSXP, n_of_rules));
    PROTECT(conf = allocVector(REALSXP, n_of_rules));
    SET_VECTOR_ELT(result, 0, support);
    SET_VECTOR_ELT(result, 1, conf);

    PROTECT(lhs_i = allocVector(INTSXP, length_lhs));
    PROTECT(lhs_p = allocVector(INTSXP, n_of_rules+1));
    PROTECT(rhs_i = allocVector(INTSXP, n_of_rules));
    PROTECT(rhs_p = allocVector(INTSXP, n_of_rules+1));
    SET_VECTOR_ELT(result, 2, lhs_i);
    SET_VECTOR_ELT(result, 3, lhs_p);
    SET_VECTOR_ELT(result, 4, rhs_i);
    SET_VECTOR_ELT(result, 5, rhs_p);
    
    INTEGER(lhs_p)[0] = 0;
    INTEGER(rhs_p)[0] = 0;
   
    PROTECT(Dim = allocVector(INTSXP, 2));
    SET_VECTOR_ELT(result, 6, Dim);

    INTEGER(Dim)[0] = n_of_items;
    INTEGER(Dim)[1] = n_of_rules;

    /* count buffer */
    buffer = (int *) R_alloc(n_of_trans, sizeof(int));
    for(i = 0; i < n_of_trans; i++) buffer[i] = 0;

    /* buffer for rule critical transactions which may increase the
       count of the lhs */ 
    hits = (int *) R_alloc(n_of_trans, sizeof(int));

    /* go through itemsets */
    /* Rprintf("doing itemset "); */
    for(i = 0; i < (n_of_sets); i++) {
        /* Rprintf("%d ", i); */
       
        /* drop 1-itemsets */
        if(i_p[i+1]-i_p[i] < 2) continue;

        /* go through itemset */
        for(j = i_p[i]; j < i_p[i+1]; j++) {
            item = i_i[j];
            /* Rprintf("\n\titem %d\n", item); */
            
            /* add individual item tidlist */
            for(k = t_p[item]; k < t_p[item+1]; k++){
                buffer[t_i[k]]++; 
            }
        }
        
        /* count support count */
        length_of_itemset = i_p[i+1]-i_p[i];
        count = 0;
        l = 0;
        for(j = 0; j < n_of_trans; j++){
            if(buffer[j] >= length_of_itemset-1) {
                if(buffer[j] == length_of_itemset) count++;
                /* now remember tids transactions which my increase the
                   count of the lhs (buffer[j] == length_of_itemset-1) */
                else hits[l++] = j; 
            }

            /* clear buffer again */
            buffer[j] = 0;
        }
        
       
        /* go again through all items for subset support */
        for(j = i_p[i]; j < i_p[i+1]; j++) {
            item = i_i[j];
            
            /*
            Rprintf("hits ");
            for(k = 0; k < l; k++){
                Rprintf("%d ", hits[k]);
            }
            Rprintf("\n");
            
            Rprintf("tidl ");
            for(k = t_p[item]; k <t_p[item+1]; k++){
                Rprintf("%d ", t_i[k]);
            }
            Rprintf("\n");
            */

            /* we start with the max possible count and subtract matches */
            subset_count = count + l;
            m = t_p[item];
            for(k = 0; k < l && m < t_p[item+1]; k++){
                while(hits[k] > t_i[m] && m < t_p[item+1]) m++;
                if(hits[k] == t_i[m]) subset_count--;
            }

            /* Rprintf("%d supp: %d lhs-supp: %d (item %d)\n", 
                    i, count, subset_count, item); 
	    */

            /* quality */
            REAL(support)[n] = (double) count / dbl_n_of_trans;
            REAL(conf)[n] = (double) count / (double) subset_count;
        
            /* rule */
            INTEGER(rhs_p)[n+1] = n+1;
            INTEGER(rhs_i)[n] = item;
            INTEGER(lhs_p)[n+1] = INTEGER(lhs_p)[n] +  
                i_p[i+1] - i_p[i] - 1;
           
            for(k = i_p[i]; k < i_p[i+1]; k++)
                if(i_i[k] != item) INTEGER(lhs_i)[lhs_i_counter++] = i_i[k];
                
            n++;
        }

        if(n%100 == 0) R_CheckUserInterrupt();
    } 
        
    UNPROTECT(8);
    return result;
}
