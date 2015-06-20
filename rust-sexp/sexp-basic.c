/* SEXP implementation code sexp-basic.c
 * List-based implementation of S-expressions.
 * Ron Rivest
 * 5/5/1997
 */

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include "sexp.h"

/******************/
/* ERROR MESSAGES */
/******************/

/* ErrorMessage(level,msg,c1,c2)
 * prints error message on standard output (msg is a c format string)
 * c1 and c2 are (optional) integer parameters for the message.
 * Terminates iff level==ERROR, otherwise returns.
 */
void ErrorMessage(level,msg,c1,c2)
int level; 
char *msg;
int c1, c2;
{
  fflush(stdout);
  printf("\n*** ");
  if (level==WARNING) printf("Warning: ");
  else if (level==ERROR) printf("Error: ");
  printf(msg,c1,c2);
  printf(" ***\n");
  if (level==ERROR) exit(1);
}

/**********************/
/* STORAGE ALLOCATION */
/**********************/

/* initializeMemory()
 * take care of memory initialization 
 */
void initializeMemory()
{ ; } /* nothing in this implementation -- use malloc */

/* sexpAlloc(n)
 * Allocates n bytes of storage. 
 * Terminates execution if no memory available.
 */
char *sexpAlloc(n)
int n;
{ char *c = (char *)malloc((unsigned int) n);
  if (c == NULL) ErrorMessage(ERROR,"Error in sexpAlloc: out of memory!",0,0);
  return(c);
}

/***********************************/
/* SEXP SIMPLE STRING MANIPULATION */
/***********************************/

/* newSimpleString()
 * Creates and initializes new sexpSimpleString object.
 * Allocates 16-character buffer to hold string.
 */
sexpSimpleString *newSimpleString()
{
  sexpSimpleString *ss;
  ss = (sexpSimpleString *) sexpAlloc(sizeof(sexpSimpleString));
  ss->length = 0;
  ss->allocatedLength = 16;
  ss->string = (octet *)sexpAlloc(16);
  return(ss);
}
			      
/* simpleStringLength(ss)
 * returns length of simple string 
 */
long int simpleStringLength(ss)
sexpSimpleString *ss;
{ return(ss->length); }

/* simpleStringString(ss)
 * returns pointer to character array of simple string 
 */
octet *simpleStringString(ss)
sexpSimpleString *ss;
{ return(ss->string); }

/* reallocateSimpleString(ss)
 * Changes space allocated to ss.
 * Space allocated is set to roughly 3/2 the current string length, plus 16.
 */
sexpSimpleString *reallocateSimpleString(ss)
sexpSimpleString *ss;
{
  int newsize, i;
  octet *newstring;
  if (ss==NULL) ss = newSimpleString();
  if (ss->string == NULL) 
    ss->string = (octet *)sexpAlloc(16);
  else
    { 
      newsize = 16 + 3*(ss->length)/2;
      newstring = (octet *)sexpAlloc(newsize);
      for (i=0;i<ss->length;i++) newstring[i] = ss->string[i];
      /* zeroize string before freeing; as it may be sensitive */
      for (i=0;i<ss->allocatedLength;i++) ss->string[i] = 0;
      free(ss->string);
      ss->string = newstring;
      ss->allocatedLength = newsize;
    }
  return(ss);
}

/* appendCharToSimpleString(c,ss)
 * Appends the character c to the end of simple string ss.
 * Reallocates storage assigned to s if necessary to make room for c.
 */
void appendCharToSimpleString(c,ss)
int c;
sexpSimpleString *ss;
{
  if (ss==NULL) ss = newSimpleString();
  if (ss->string == NULL || ss->length == ss->allocatedLength )
    ss = reallocateSimpleString(ss);
  ss->string[ss->length] = (octet) (c & 0xFF);
  ss->length++;
}

/****************************/
/* SEXP STRING MANIPULATION */
/****************************/

/* newSexpString()
 * Creates and initializes a new sexpString object.
 * Both the presentation hint and the string are initialized to NULL.
 */
sexpString *newSexpString()
{
  sexpString *s;
  s = (sexpString *) sexpAlloc(sizeof(sexpString));
  s->type = SEXP_STRING;
  s->presentationHint = NULL;
  s->string = NULL;
  return(s);
}

/* sexpStringPresentationHint()
 * returns presentation hint field of the string 
 */
sexpSimpleString *sexpStringPresentationHint(s)
sexpString *s;
{ return(s->presentationHint); }

/* setSexpStringPresentationHint()
 * assigns the presentation hint field of the string
 */
void setSexpStringPresentationHint(s,ss)
sexpString *s;
sexpSimpleString *ss;
{ s->presentationHint = ss; }

/* setSexpStringString()
 * assigns the string field of the string
 */
void setSexpStringString(s,ss)
sexpString *s;
sexpSimpleString *ss;
{ s->string = ss; }

/* sexpStringString()
 * returns the string field of the string
 */
sexpSimpleString *sexpStringString(s)
sexpString *s;
{ return(s->string); }

/* closeSexpString()
 * finish up string computations after created 
 */
void closeSexpString(s)
sexpString *s;
{ ; }  /* do nothing in this implementation */

/**************************/
/* SEXP LIST MANIPULATION */
/**************************/

/* newSexpList()
 * Creates and initializes a new sexpList object.
 * Both the first and rest fields are initialized to NULL, which is
 * SEXP's representation of an empty list.
 */
sexpList *newSexpList()
{
  sexpList *list;
  list = (sexpList *) sexpAlloc(sizeof(sexpList));
  list->type = SEXP_LIST;
  list->first = NULL;
  list->rest = NULL;
  return(list);
}

/* sexpAddSexpListObject()
 * add object to end of list
 */
void sexpAddSexpListObject(list,object)
sexpList *list;
sexpObject *object;
{
  if (list->first == NULL)
    list->first = object;
  else
    { while (list->rest != NULL) list = list->rest;
      list->rest = newSexpList();
      list = list->rest;
      list->first = object;
    }
}

/* closeSexpList()
 * finish off a list that has just been input
 */
void closeSexpList(list)
sexpList *list;
{ ; } /* nothing in this implementation */

/* Iteration on lists.
   To accomodate different list representations, we introduce the
   notion of an "iterator".
*/

/* sexpListIter()
 * return the iterator for going over a list 
 */
sexpIter *sexpListIter(list)
sexpList *list;
{ return((sexpIter *)list); }

/* sexpIterNext()
 * advance iterator to next element of list, or else return null
 */
sexpIter *sexpIterNext(iter)
sexpIter *iter;
{ if (iter == NULL) return(NULL);
  return((sexpIter *)(((sexpList *)iter)->rest));
}

/* sexpIterObject ()
 * return object corresponding to current state of iterator
 */
sexpObject *sexpIterObject(iter)
sexpIter *iter;
{ if (iter == NULL) return(NULL);
  return(((sexpList *)iter)->first);
}

/****************************/
/* SEXP OBJECT MANIPULATION */
/****************************/

int isObjectString(object)
sexpObject *object;
{ if (((sexpString *)object)->type == SEXP_STRING) return(TRUE);
  else                                             return(FALSE);
}

int isObjectList(object)
sexpObject *object;
{ if (((sexpList *)object)->type == SEXP_LIST) return(TRUE);
  else                                         return(FALSE);
}

