/* libavl - manipulates AVL trees.
   Copyright (C) 1998, 1999 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.

   The author may be contacted at <pfaffben@pilot.msu.edu> on the
   Internet, or as Ben Pfaff, 12167 Airport Rd, DeWitt MI 48820, USA
   through more mundane means. */

/* This is file avltr.c in libavl. */
/* Martin Schlather: 21 October, 2011,
   This file has been file avltr.cc in libavl. 
   The #include "avltr.h" has been changed to "avltr_modified.h".
   Further
       #include "basic.h" is added,
       fprintf has been replaced by error,
       exit has been outcommented,
       printf has been replaced by Rprintf.
   The above stated GNU General Public License to its full extend
   is still valid.
*/


#if HAVE_CONFIG_H
#include <config.h>
#endif
#if SELF_TEST 
#include <limits.h>
#include <time.h>
#endif
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "basic.h" // must be before assert.h
 
//#include "avltr.h"
#include "avltr.h"

/* Tag types. */
#define PLUS +1
#define MINUS -1

#if !__GCC__ && !defined (inline)
#define inline
#endif

void
print_structure (avltr_tree *tree, avltr_node *node, int level);

#if __GNUC__ >= 2
#define unused __attribute__ ((unused))
#else
#define unused
#endif

#ifdef HAVE_XMALLOC
void *xmalloc (size_t);
#else /* !HAVE_XMALLOC */
/* Allocates SIZE bytes of space using malloc().  Aborts if out of
   memory. */
static void *
xmalloc (size_t size)
{
  void *vp;
  if (size == 0)
    return NULL;
  vp = malloc (size);
  assert (vp != NULL);
  if (vp == NULL)
    {
      error("virtual memory exhausted\n");
      // exit (EXIT_FAILURE);
    }
  return vp;
}
#endif /* !HAVE_XMALLOC */

/* Creates an AVL tree in arena OWNER (which can be NULL).  The arena
   is owned by the caller, not by the AVL tree.  CMP is a order
   function for the data to be stored in the tree.  PARAM is arbitrary
   data that becomes an argument to the comparison function. */
avltr_tree *
avltr_create (avl_comparison_func cmp, void *param)
{
  avltr_tree *tree;

  assert (cmp != NULL);
  tree = (avltr_tree *) xmalloc (sizeof (avltr_tree));

  tree->root.link[0] = NULL;
  tree->root.link[1] = &tree->root;
  tree->root.rtag = PLUS;
  tree->cmp = cmp;
  tree->count = 0;
  tree->param = param;

  return tree;
}

/* Destroy tree TREE.  Function FREE_FUNC is called for every node in
   the tree as it is destroyed.  

   No effect if the tree has an arena owner and free_func is NULL.
   The caller owns the arena and must destroy it itself.

   Do not attempt to reuse the tree after it has been freed.  Create a
   new one.  */
void
avltr_destroy (avltr_tree *tree, avl_node_func free_func)
{
  assert (tree != NULL);
  
  if (tree->root.link[0] != &tree->root)
    {
      /* Uses Knuth's Algorithm 2.3.1T as modified in exercise 13
	 (postorder traversal), further modified for right-threaded
	 trees. */
      
      /* T1. */
      avltr_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
      char ab[AVL_MAX_HEIGHT];		/* Stack A: bits. */
      int ap = 0;			/* Stack A: height. */
      avltr_node *p = tree->root.link[0];

      for (;;)
	{
	  /* T2. */
	  while (p != NULL)
	    {
	      /* T3. */
	      ab[ap] = 0;
	      an[ap++] = p;
	      p = p->link[0];
	    }

	  /* T4. */
	  for (;;)
	    {
	      if (ap == 0)
		goto done;

	      p = an[--ap];
	      if (ab[ap] == 0)
		{
		  ab[ap++] = 1;
		  if (p->rtag == MINUS)
		    continue;
		  p = p->link[1];
		  break;
		}
      
	      if (free_func)
	      	free_func (p->data, tree->param);
	      free (p);
	    }
	}
    }

 done:
  free (tree);
}

/* avltr_destroy() with FREE_FUNC hardcoded as free(). */
void
avltr_free (avltr_tree *tree)
{
  avltr_destroy (tree, (avl_node_func) free);
}

/* Return the number of nodes in TREE. */
int
avltr_count (const avltr_tree *tree)
{
  assert (tree != NULL);
  return tree->count;
}

/* Copy the contents of TREE to a new tree in arena OWNER.  If COPY is
   non-NULL, then each data item is passed to function COPY, and the
   return values are inserted into the new tree; otherwise, the items
   are copied verbatim from the old tree to the new tree.  Returns the
   new tree. */
avltr_tree *
avltr_copy (const avltr_tree *tree, avl_copy_func copy)
{
  /* Knuth's Algorithm 2.3.1C (copying a binary tree).  Additionally
     uses Algorithm 2.3.1I (insertion into a threaded binary tree) and
     Algorithm 2.3.1 exercise 17 (preorder successor in threaded
     binary tree).  */

  avltr_tree *new_tree;

  const avltr_node *p;
  avltr_node *q;
  
  assert (tree != NULL);
  new_tree = avltr_create (tree->cmp, tree->param);
  new_tree->count = tree->count;
  p = &tree->root;
  if (p->link[0] == p)
    return new_tree;
  q = &new_tree->root;

  for (;;)
    {
      /* C4.  This is Algorithm 2.3.1 exercise 23 for insertion to the
       left in a right-threaded binary tree. */
      if (p->link[0] != NULL)
	{
	  avltr_node *r = (avltr_node *) xmalloc (sizeof (avltr_node));

	  q->link[0] = r;
	  r->link[0] = NULL;
	  r->link[1] = q;
	  r->rtag = MINUS;
	}

      /* C5: Find preorder successors of P and Q.  This is Algorithm
         2.3.1 exercise 17 but applies its actions to Q as well as
         P. */
      if (p->link[0] != NULL)
	{
	  p = p->link[0];
	  q = q->link[0];
	}
      else
	{
	  while (p->rtag == MINUS)
	    {
	      p = p->link[1];
	      q = q->link[1];
	    }
	  p = p->link[1];
	  q = q->link[1];
	}

      /* C6. */
      if (p == &tree->root)
	{
	  assert (q == &new_tree->root);
	  return new_tree;
	}
      
      /* C2.  This is Algorithm 2.3.1 exercise 23 for insertion to the
	 right in a right-threaded binary tree. */
      if (p->rtag == PLUS)
	{
	  avltr_node *r =  (avltr_node *) xmalloc (sizeof (avltr_node));

	  r->link[1] = q->link[1];
	  r->rtag = q->rtag;
	  q->link[1] = r;
	  q->rtag = PLUS;
	  r->link[0] = NULL;
	}

      /* C3. */
      q->bal = p->bal;
      if (copy == NULL)
	q->data = p->data;
      else
	q->data = copy (p->data, tree->param);
    }
}

/* Threads the unthreaded AVL tree TREE in-place, and returns TREE cast to
   avltr_tree *. */
avltr_tree *
avltr_thread (struct avl_tree *_tree)
{
  /* Uses Knuth's Algorithm 2.3.1 exercise 30 (thread an unthreaded
     tree, with Algorithm 2.3.1T (inorder traversal) for computing
     Q$. */

  avltr_tree *tree = (avltr_tree *) _tree;

  /* Algorithm T's variables. */
  avltr_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
  avltr_node **ap;			/* Stack A: stack pointer. */
  avltr_node *tp;			/* P. */

  /* Algorithm L's variables. */
  avltr_node *p, *q;

  assert (tree != NULL);

  /* T1. */
  ap = an;
  tp = tree->root.link[0];

  /* L1. */
  q = &tree->root;
  q->link[1] = q;

  for (;;)
    {
      /* L2. */
      {
	/* T2. */
	while (tp != NULL)
	  {
	    /* T3. */
	    *ap++ = tp;
	    tp = tp->link[0];
	  }
      
	/* T4.  Modified to visit HEAD after fully traversing the
           tree. */
	if (ap == an)
	  tp = &tree->root;
	else
	  tp = *--ap;

	/* T5: Visit P. */
	p = tp;
      }

      /* L3. */
      if (q->link[1] == NULL)
	{
	  q->link[1] = p;
	  q->rtag = MINUS;
	}
      else
	q->rtag = PLUS;
  
      /* L4. */
      if (p == &tree->root)
	return tree;
      q = p;

      /* T5: Next. */
      tp = tp->link[1];
    }
}

/* Unthreads the threaded tree TREE in-place, and returns TREE cast to
   avl_tree *. */
struct avl_tree *
avltr_unthread (avltr_tree *tree)
{
  /* Uses Knuth's Algorithm 2.3.1T as modified in exercise 13
     (postorder traversal). */
      
  /* T1. */
  avltr_node *an[AVL_MAX_HEIGHT];	/* Stack A: nodes. */
  char ab[AVL_MAX_HEIGHT];		/* Stack A: bits. */
  int ap = 0;				/* Stack A: height. */
  avltr_node *p;

  assert (tree != NULL);
  p = tree->root.link[0];
  if (p != NULL)
    for (;;)
      {
	/* T2. */
	for (;;)
	  {
	    /* T3. */
	    ab[ap] = 0;
	    an[ap++] = p;
	    if (p->link[0] == NULL)
	      break;
	    p = p->link[0];
	  }

	/* T4. */
	for (;;)
	  {
	    if (ap == 0)
	      goto done;

	    p = an[--ap];
	    if (ab[ap] == 0)
	      {
		ab[ap++] = 1;
		if (p->rtag == MINUS)
		  continue;
		p = p->link[1];
		break;
	      }
      
	    if (p->rtag == MINUS)
	      p->link[1] = NULL;
	  }
      }
  else
    tree->root.link[0] = NULL;

 done:
  tree->root.link[1] = NULL;
  return (struct avl_tree *) tree;
}

/* Walk tree TREE in inorder, calling WALK_FUNC at each node.  Passes
   PARAM to WALK_FUNC.  */
void
avltr_walk (const avltr_tree *tree, avl_node_func walk_func, void *param)
{
  const avltr_node *p = &tree->root;

  /* Uses Knuth's algorithm 2.3.1D (threaded inorder successor). */
  assert (tree && walk_func);
  
  for (;;)
    {
      if (p->rtag == MINUS)
	p = p->link[1];
      else
	{
	  p = p->link[1];
	  while (p->link[0] != NULL)
	    p = p->link[0];
	}

      if (p == &tree->root)
	return;

      walk_func (p->data, param);
    }
}

/* Each call to this function for a given TREE and TRAV return the
   next item in the tree in inorder.  Initialize the first element of
   TRAV (init) to 0 before calling the first time.  Returns NULL when
   out of elements.  */
void *
avltr_traverse (const avltr_tree *tree, avltr_traverser *trav)
{
  const avltr_node *p;
  
  assert (tree && trav);

  if (trav->init == 0)
    {
      p = &tree->root;
      trav->init = 1;
    }
  else
    p = trav->p;

  /* Knuth's Algorithm 2.3.1S (threaded inorder successor). */
  if (p->rtag == MINUS)
    p = p->link[1];
  else
    {
      p = p->link[1];
      while (p->link[0] != NULL)
	p = p->link[0];
    }

  if (p == &tree->root)
    {
      trav->init = 0;
      return NULL;
    }
  else
    {
      trav->p = p;
      return (void *) p->data;
    }
}

/* Given ITEM, a pointer to a data item in TREE (or NULL), returns a
   pointer to the next item in the tree in comparison order, or NULL
   if ITEM is the last item. */
void **
avltr_next (const avltr_tree *tree, void **item)
{
  const avltr_node *p;

  assert (tree != NULL);
  if (item == NULL)
    p = &tree->root;
  else
    p = (avltr_node *) (((char *) item) - offsetof (avltr_node, data));

  /* Knuth's Algorithm 2.3.1S (threaded inorder successor). */
  if (p->rtag == MINUS)
    p = p->link[1];
  else
    {
      p = p->link[1];
      while (p->link[0] != NULL)
	p = p->link[0];
    }

  if (p == &tree->root)
    return NULL;

  return (void **) &p->data;
}

/* Search TREE for an item matching ITEM.  If found, returns a pointer
   to the address of the item.  If none is found, ITEM is inserted
   into the tree, and a pointer to the address of ITEM is returned.
   In either case, the pointer returned can be changed by the caller,
   or the returned data item can be directly edited, but the key data
   in the item must not be changed. */
void **
avltr_probe (avltr_tree *tree, void *item)
{
  /* Uses Knuth's Algorithm 6.2.3A (balanced tree search and
     insertion), modified for a right-threaded binary tree.  Caches
     results of comparisons.  In empirical tests this eliminates about
     25% of the comparisons seen under random insertions.  */

  /* A1. */
  avltr_node *t;
  avltr_node *s, *p, *q, *r;

  assert (tree != NULL);
  t = &tree->root;
  s = p = t->link[0];

  if (s == NULL)
    {
      tree->count++;
      assert (tree->count == 1);
      q = t->link[0] = (avltr_node *) xmalloc (sizeof (avltr_node));
      q->data = item;
      q->link[0] = NULL;
      q->link[1] = t;
      q->rtag = MINUS;
      q->bal = 0;
      return &q->data;
    }

  for (;;)
    {
      /* A2. */
      int diff = tree->cmp (item, p->data, tree->param);

      /* A3. */
      if (diff < 0)
	{
	  p->cache = 0;
	  q = p->link[0];
	  if (q == NULL)
	    {
	      /* Algorithm 2.3.1 exercise 23 for insertion to the left
		 in a right-threaded binary tree. */
	      q = (avltr_node *) xmalloc (sizeof (avltr_node));
	      p->link[0] = q;
	      q->link[0] = NULL;
	      q->link[1] = p;
	      q->rtag = MINUS;
	      break;
	    }
	}
      /* A4. */
      else if (diff > 0)
	{
	  p->cache = 1;
	  q = p->link[1];
	  if (p->rtag == MINUS)
	    {
	      /* Algorithm 2.3.1 exercise 23 for insertion to the right
		 in a right-threaded binary tree. */
	      q = (avltr_node *) xmalloc (sizeof (avltr_node));
	      q->link[1] = p->link[1];
	      q->rtag = p->rtag;
	      p->link[1] = q;
	      p->rtag = PLUS;
	      q->link[0] = NULL;
	      break;
	    }
	  assert (q != NULL);
	}
      else
	/* A2. */
	return &p->data;

      /* A3, A4. */
      if (q->bal != 0)
	t = p, s = q;
      p = q;
    }
  
  /* A5. */
  tree->count++;
  q->data = item;
  q->bal = 0;
  
  /* A6. */
  r = p = s->link[(int) s->cache];
  while (p != q)
    {
      p->bal = p->cache * 2 - 1;
      p = p->link[(int) p->cache];
    }

  /* A7. */
  if (s->cache == 0)
    {
      /* a = -1. */
      if (s->bal == 0)
	{
	  s->bal = -1;
	  return &q->data;
	}
      else if (s->bal == +1)
	{
	  s->bal = 0;
	  return &q->data;
	}

      assert (s->bal == -1);
      if (r->bal == -1)
	{
	  /* A8. */
	  p = r;
	  if (r->rtag == MINUS)
	    {
	      s->link[0] = NULL;
	      r->link[1] = s;
	      r->rtag = PLUS;
	    }
	  else
	    {
	      s->link[0] = r->link[1];
	      r->link[1] = s;
	    }
	  s->bal = r->bal = 0;
	}
      else
	{
	  /* A9. */
	  assert (r->bal == +1);
	  p = r->link[1];
	  r->link[1] = p->link[0];
	  p->link[0] = r;
	  s->link[0] = p->link[1];
	  p->link[1] = s;
	  if (p->bal == -1)
	    s->bal = 1, r->bal = 0;
	  else if (p->bal == 0)
	    s->bal = r->bal = 0;
	  else 
	    {
	      assert (p->bal == +1);
	      s->bal = 0, r->bal = -1;
	    }
	  p->bal = 0;
	  p->rtag = PLUS;
	  if (s->link[0] == s)
	    s->link[0] = NULL;
	  if (r->link[1] == NULL)
	    {
	      r->link[1] = p;
	      r->rtag = MINUS;
	    }
	}
    }
  else
    {
      /* a == +1. */
      if (s->bal == 0)
	{
	  s->bal = 1;
	  return &q->data;
	}
      else if (s->bal == -1)
	{
	  s->bal = 0;
	  return &q->data;
	}

      assert (s->bal == +1);
      if (r->bal == +1)
	{
	  /* A8. */
	  p = r;
	  if (r->link[0] == NULL)
	    {
	      s->rtag = MINUS;
	      r->link[0] = s;
	    }
	  else
	    {
	      s->link[1] = r->link[0];
	      s->rtag = PLUS;
	      r->link[0] = s;
	    }
	  s->bal = r->bal = 0;
	}
      else
	{
	  /* A9. */
	  assert (r->bal == -1);
	  p = r->link[0];
	  r->link[0] = p->link[1];
	  p->link[1] = r;
	  s->link[1] = p->link[0];
	  p->link[0] = s;
	  if (p->bal == +1)
	    s->bal = -1, r->bal = 0;
	  else if (p->bal == 0)
	    s->bal = r->bal = 0;
	  else 
	    {
	      assert (p->bal == -1);
	      s->bal = 0, r->bal = 1;
	    }
	  p->rtag = PLUS;
	  if (s->link[1] == NULL)
	    {
	      s->link[1] = p;
	      s->rtag = MINUS;
	    }
	  if (r->link[0] == r)
	    r->link[0] = NULL;
	  p->bal = 0;
	}
    }
		
  /* A10. */
  if (t != &tree->root && s == t->link[1])
    t->link[1] = p;
  else
    t->link[0] = p;

  return &q->data;
}
  
/* Search TREE for an item matching ITEM, and return a pointer to it
   if found. */
void **
avltr_find (const avltr_tree *tree, const void *item)
{
  const avltr_node *p;

  assert (tree != NULL);
  p = tree->root.link[0];
  if (p == NULL)
    return NULL;
  for (;;)
    {
      int diff = tree->cmp (item, p->data, tree->param);

      /* A3. */
      if (diff < 0)
	{
	  p = p->link[0];
	  if (p == NULL)
	    return NULL;
	}
      else if (diff > 0)
	{
	  if (p->rtag == MINUS)
	    return NULL;
	  p = p->link[1];
	}
      else
	return (void **) &p->data;
    }
}

/* Search TREE for an item close to the value of ITEM, and return it.
   This function will return a null pointer only if TREE is empty. */
void **
avltr_find_close (const avltr_tree *tree, const void *item)
{
  const avltr_node *p;

  assert (tree != NULL);
  p = tree->root.link[0];
  if (p == NULL)
    return NULL;
  for (;;)
    {
      int diff = tree->cmp (item, p->data, tree->param);

      /* A3. */
      if (diff < 0)
	{
	  if (p->link[0])
	    p = p->link[0];
	  else
	    return (void **) &p->data;
	}
      else if (diff > 0)
	{
	  if (p->rtag == MINUS)
	    return (void **) &p->data;
	  p = p->link[1];
	}
      else
	return (void **) &p->data;
    }
}

/* Searches AVL tree TREE for an item matching ITEM.  If found, the
   item is removed from the tree and the actual item found is returned
   to the caller.  If no item matching ITEM exists in the tree,
   returns NULL. */
void *
avltr_delete (avltr_tree *tree, const void *item)
{
  /* Uses my Algorithm DTR, which can be found at
     http://www.msu.edu/user/pfaffben/avl.  Algorithm DT is based on
     Knuth's Algorithms 6.2.2D (Tree deletion), 6.2.3A (Balanced tree
     search and insertion), 2.3.1I (Insertion into a threaded binary
     trees), and the notes on pages 465-466 of Vol. 3. */

  /* D1. */
  avltr_node *pa[AVL_MAX_HEIGHT];	/* Stack P: Nodes. */
  unsigned char a[AVL_MAX_HEIGHT];	/* Stack P: Bits. */
  int k = 1;				/* Stack P: Pointer. */
  
  avltr_node *p;

  assert (tree != NULL);

  a[0] = 0;
  pa[0] = &tree->root;
  p = tree->root.link[0];
  if (p == NULL)
    return NULL;
  for (;;)
    {
      /* D2. */
      int diff = tree->cmp (item, p->data, tree->param);

      if (diff == 0)
	break;

      /* D3, D4. */
      pa[k] = p;
      if (diff < 0)
	{
	  if (p->link[0] != NULL)
	    {
	      p = p->link[0];
	      a[k] = 0;
	    }
	  else
	    return NULL;
	}
      else if (diff > 0)
	{
	  if (p->rtag == PLUS)
	    {
	      p = p->link[1];
	      a[k] = 1;
	    }
	  else
	    return NULL;
	}

      k++;
    }
  tree->count--;
  
  item = p->data;

  {
    avltr_node *t = p;
    avltr_node **q = &pa[k - 1]->link[(int) a[k - 1]];

    /* D5. */
    if (t->rtag == MINUS)
      {
	if (t->link[0] != NULL)
	  {
	    avltr_node *const x = t->link[0];

	    *q = x;
	    (*q)->bal = 0;
	    if (x->rtag == MINUS)
	      {
		if (a[k - 1] == 1)
		  x->link[1] = t->link[1];
		else
		  x->link[1] = pa[k - 1];
	      }
	  }
	else
	  {
	    *q = t->link[a[k - 1]];
	    if (a[k - 1] == 0)
	      pa[k - 1]->link[0] = NULL;
	    else
	      pa[k - 1]->rtag = MINUS;
	  }
      }
    else
      {
	/* D6. */
	avltr_node *r = t->link[1];
	if (r->link[0] == NULL)
	  {
	    r->link[0] = t->link[0];
	    r->bal = t->bal;
	    if (r->link[0] != NULL)
	      {
		avltr_node *s = r->link[0];
		while (s->rtag == PLUS)
		  s = s->link[1];
		assert (s->rtag == MINUS);
		s->link[1] = r;
	      }
	    *q = r;
	    a[k] = 1;
	    pa[k++] = r;
	  }
	else
	  {
	    /* D7. */
	    avltr_node *s = r->link[0];

	    a[k] = 1;
	    pa[k++] = t;

	    a[k] = 0;
	    pa[k++] = r;
	    
	    /* D8. */
	    while (s->link[0] != NULL)
	      {
		r = s;
		s = r->link[0];
		a[k] = 0;
		pa[k++] = r;
	      }

	    /* D9. */
	    t->data = s->data;
	    if (s->rtag == PLUS)
	      r->link[0] = s->link[1];
	    else
	      r->link[0] = NULL;
	    p = s;
	  }
      }
  }

  free (p);

  assert (k > 0);
  /* D10. */
  while (--k)
    {
      avltr_node *const s = pa[k];

      if (a[k] == 0)
	{
	  avltr_node *const r = s->link[1];
	  
	  /* D10. */
	  if (s->bal == -1)
	    {
	      s->bal = 0;
	      continue;
	    }
	  else if (s->bal == 0)
	    {
	      s->bal = +1;
	      break;
	    }

	  assert (s->bal == +1);
	  if (s->rtag == MINUS || r->bal == 0)
	    {
	      /* D11. */
	      s->link[1] = r->link[0];
	      r->link[0] = s;
	      r->bal = -1;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	      break;
	    }
	  else if (r->bal == +1)
	    {
	      /* D12. */
	      if (r->link[0] != NULL)
		{
		  s->rtag = PLUS;
		  s->link[1] = r->link[0];
		}
	      else
		s->rtag = MINUS;
	      r->link[0] = s;
	      s->bal = r->bal = 0;
	      pa[k - 1]->link[a[k - 1]] = r;
	    }
	  else 
	    {
	      /* D13. */
	      assert (r->bal == -1);
	      p = r->link[0];
	      if (p->rtag == PLUS)
		r->link[0] = p->link[1];
	      else
		r->link[0] = NULL;
	      p->link[1] = r;
	      p->rtag = PLUS;
	      if (p->link[0] == NULL)
		{
		  s->link[1] = p;
		  s->rtag = MINUS;
		}
	      else
		{
		  s->link[1] = p->link[0];
		  s->rtag = PLUS;
		}
	      p->link[0] = s;
	      if (p->bal == +1)
		s->bal = -1, r->bal = 0;
	      else if (p->bal == 0)
		s->bal = r->bal = 0;
	      else
		{
		  assert (p->bal == -1);
		  s->bal = 0, r->bal = +1;
		}
	      p->bal = 0;
	      pa[k - 1]->link[(int) a[k - 1]] = p;
	      if (a[k - 1] == 1)
		pa[k - 1]->rtag = PLUS;
	    }
	}
      else
	{
	  avltr_node *const r = s->link[0];
	  
	  /* D10. */
	  if (s->bal == +1)
	    {
	      s->bal = 0;
	      continue;
	    }
	  else if (s->bal == 0)
	    {
	      s->bal = -1;
	      break;
	    }

	  assert (s->bal == -1);
	  if (s->link[0] == NULL || r->bal == 0)
	    {
	      /* D11. */
	      s->link[0] = r->link[1];
	      r->link[1] = s;
	      r->bal = +1;
	      pa[k - 1]->link[(int) a[k - 1]] = r;
	      break;
	    }
	  else if (r->bal == -1)
	    {
	      /* D12. */
	      if (r->rtag == PLUS)
		s->link[0] = r->link[1];
	      else
		s->link[0] = NULL;
	      r->link[1] = s;
	      r->rtag = PLUS;
	      s->bal = r->bal = 0;
	      pa[k - 1]->link[a[k - 1]] = r;
	    }
	  else 
	    {
	      /* D13. */
	      assert (r->bal == +1);
	      p = r->link[1];
	      if (p->link[0] != NULL)
		{
		  r->rtag = PLUS;
		  r->link[1] = p->link[0];
		}
	      else
		r->rtag = MINUS;
	      p->link[0] = r;
	      if (p->rtag == MINUS)
		s->link[0] = NULL;
	      else
		s->link[0] = p->link[1];
	      p->link[1] = s;
	      p->rtag = PLUS;
	      if (p->bal == -1)
		s->bal = +1, r->bal = 0;
	      else if (p->bal == 0)
		s->bal = r->bal = 0;
	      else
		{
		  assert (p->bal == +1);
		  s->bal = 0, r->bal = -1;
		}
	      p->bal = 0;
	      if (a[k - 1] == 1)
		pa[k - 1]->rtag = PLUS;
	      pa[k - 1]->link[(int) a[k - 1]] = p;
	    }
	}
    }
      
  return (void *) item;
}

/* Inserts ITEM into TREE.  Returns NULL if the item was inserted,
   otherwise a pointer to the duplicate item. */
void *
avltr_insert (avltr_tree *tree, void *item)
{
  void **p;
  
  assert (tree != NULL);
  
  p = avltr_probe (tree, item);
  return (*p == item) ? NULL : *p;
}

/* If ITEM does not exist in TREE, inserts it and returns NULL.  If a
   matching item does exist, it is replaced by ITEM and the item
   replaced is returned.  The caller is responsible for freeing the
   item returned. */
void *
avltr_replace (avltr_tree *tree, void *item)
{
  void **p;

  assert (tree != NULL);
  
  p = avltr_probe (tree, item);
  if (*p == item)
    return NULL;
  else
    {
      void *r = *p;
      *p = item;
      return r;
    }
}

/* Delete ITEM from TREE when you know that ITEM must be in TREE.  For
   debugging purposes. */
void *
(avltr_force_delete) (avltr_tree *tree, void *item)
{
  void *found = avltr_delete (tree, item);
  assert (found != NULL);
  return found;
}

#if SELF_TEST

/* Size of the tree used for testing. */
#define TREE_SIZE 1024

/* Used to flag delayed aborting. */
int done = 0;

/* Count the number of nodes in TREE below and including NODE. */
int
count (avltr_tree *tree, avltr_node *node)
{
  int n = 1;
  if (node->link[0] != NULL)
    n += count (tree, node->link[0]);
  if (node->rtag == PLUS)
    n += count (tree, node->link[1]);
  return n;
}

/* Print the structure of node NODE of an avl tree, which is LEVEL
   levels from the top of the tree.  Uses different delimiters to
   visually distinguish levels. */
void
print_structure (avltr_tree *tree, avltr_node *node, int level)
{
  char lc[] = "([{<`";
  char rc[] = ")]}>'";

  if (node == NULL)
    {
      Rprintf (" :nil");
      return;
    }
  else if (level >= 10)
    {
      Rprintf ("Too deep, giving up.\n");
      done = 1;
      return;
    }
  else if (node == &tree->root)
    {
      Rprintf (" root");
      return;
    }
  Rprintf (" %c%d", lc[level % 5], (int) node->data);
  fflush (stdout);

  print_structure (tree, node->link[0], level + 1);
  fflush (stdout);

  if (node->rtag == PLUS)
    print_structure (tree, node->link[1], level + 1);
  else if (node->link[1] != &tree->root)
    Rprintf (" :%d", (int) node->link[1]->data);
  else
    Rprintf (" :r");
  fflush (stdout);

  Rprintf ("%c", rc[level % 5]);
  fflush (stdout);
}

/* Compare two integers A and B and return a strcmp()-type result. */
int
compare_ints (const void *a, const void *b, void *param unused)
{
  return ((int) a) - ((int) b);
}

/* Print the value of integer A. */
void
print_int (void *a, void *param unused)
{
  Rprintf (" %d", (int) a);
}

/* Linearly print contents of TREE. */
void
print_contents (avltr_tree *tree)
{
  avltr_walk (tree, print_int, NULL);
  Rprintf ("\n");
}

/* Examine NODE in a avl tree.  *COUNT is increased by the number of
   nodes in the tree, including the current one.  If the node is the
   root of the tree, PARENT should be INT_MIN, otherwise it should be
   the parent node value.  DIR is the direction that the current node
   is linked from the parent: -1 for left child, +1 for right child;
   it is not used if PARENT is INT_MIN.  Returns the height of the
   tree rooted at NODE. */
int
recurse_tree (avltr_tree *tree, avltr_node *node, int *count, int parent,
	      int dir, unsigned char *nodes, unsigned char *threads)
{
  if (node != NULL && node != &tree->root) 
    {
      int d = (int) node->data;
      int nl = 0;
      int nr = 0;

      (*count)++;

      assert (d >= 0 && d < TREE_SIZE);
      if (nodes[d / 8] & (1 << (d % 8)))
	{
	  Rprintf (" Arrived at node %d by two different paths.\n", d);
	  done = 1;
	}
      else
	nodes[d / 8] |= 1 << (d % 8);

      if (node->link[0] != NULL)
	nl = recurse_tree (tree, node->link[0], count, d, -1, nodes, threads);

      if (node->rtag == PLUS)
	{
	  if (node->link[1] == NULL)
	    {
	      Rprintf (" Null thread link.\n");
	      done = 1;
	    }
	  nr = recurse_tree (tree, node->link[1], count, d, 1, nodes, threads);
	}
      else if (node->link[1] != &tree->root)
	{
	  int dr = (int) node->link[1]->data;
	  assert (dr >= 0 && dr < TREE_SIZE);
	  if (threads[dr / 8] & (1 << dr % 8))
	    {
	      Rprintf (" Multiple threads to node %d.\n", d);
	      done = 1;
	    }
	  threads[dr / 8] |= 1 << (dr % 8);
	}

      if (nr - nl != node->bal)
	{
	  Rprintf (" Node %d has incorrect balance: right height=%d, "
		  "left height=%d, difference=%d, but balance factor=%d.\n",
		  d, nr, nl, nr - nl, node->bal);
	  done = 1;
	}
      
      if (node->bal < -1 || node->bal > 1)
	{
	  Rprintf (" Node %d has invalid balance factor %d.\n", d, node->bal);
	  done = 1;
	}
      
      if (parent != INT_MIN)
	{
	  assert (dir == -1 || dir == +1);
	  if (dir == -1 && d > parent)
	    {
	      Rprintf (" Node %d is smaller than its left child %d.\n",
		      parent, d);
	      done = 1;
	    }
	  else if (dir == +1 && d < parent)
	    {
	      Rprintf (" Node %d is larger than its right child %d.\n",
		      parent, d);
	      done = 1;
	    }
	}
      assert (node->bal >= -1 && node->bal <= 1);
      return 1 + (nl > nr ? nl : nr);
    }
  else return 0;
}

/* Check that everything about TREE is kosher. */
void
verify_tree (avltr_tree *tree)
{
  {
    unsigned char nodes[(TREE_SIZE + 7) / 8];
    unsigned char threads[(TREE_SIZE + 7) / 8];

    int count = 0;
    int i;
    
    memset (nodes, 0, (TREE_SIZE + 7) / 8);
    memset (threads, 0, (TREE_SIZE + 7) / 8);
  
    recurse_tree (tree, tree->root.link[0], &count, INT_MIN, 0, nodes,
		  threads);
    
    if (count != tree->count)
      {
	Rprintf (" Tree should have %d nodes, but tree count by recursive "
		"descent is %d.\n", tree->count, count);
	done = 1;
      }

    for (i = 0; i < TREE_SIZE; i++)
      {
	int thread = threads[i / 8] & (1 << (i % 8));
	int node = nodes[i / 8] & (1 << (i % 8));

	if (thread && !node)
	  {
	    Rprintf (" A thread leads to ``node'' %d, "
		    "which is not in the tree.", i);
	    done = 1;
	  }
      }
  }

  /* Check threads. */
  {
    int count = 0;
    int last = INT_MIN;
    void **data = NULL;
  
    while (NULL != (data = avltr_next (tree, data)))
      {
	if (((int) *data) < last)
	  {
	    Rprintf (" Misordered right threads.\n");
	    abort ();
	  }
	else if (((int) *data) == last)
	  {
	    Rprintf (" Loop in right threads detected on %d.\n", last);
	    abort ();
	  }
	last = (int) *data;
	count++;
      }

    if (count != tree->count)
      {
	Rprintf (" Tree should have %d nodes, but tree count by right threads "
		"is %d.\n", tree->count, count);
	done = 1;
      }
  }

  if (done)
    abort ();
}

/* Arrange the N elements of ARRAY in random order. */
void
shuffle (int *array, int n)
{
  int i;
  
  for (i = 0; i < n; i++)
    {
      int j = i + rand () % (n - i);
      int t = array[j];
      array[j] = array[i];
      array[i] = t;
    }
}

/* Compares avl trees rooted at A and B, making sure that they are
   identical. */
void
compare_trees (avltr_node *a, avltr_node *b)
{
  int diff = 0;
  
  assert (a && b);
  
  /* Separating these conditions makes it easier to pinpoint bad data
     under a memory debugger like Checker because each test is a
     separate statement. */
  diff |= a->data != b->data;
  diff |= a->bal != b->bal;
  diff |= ((a->link[0] != NULL) ^ (b->link[0] != NULL));
  diff |= ((a->rtag == PLUS) ^ (b->rtag == PLUS));
  if (diff)
    {
      Rprintf (" Copied nodes differ: %d b=%d a->bal=%d b->bal=%d a:",
	      (int) a->data, (int) b->data, a->bal, b->bal);
      if (a->link[0])
	Rprintf ("l");
      if (a->link[1])
	Rprintf ("r");
      Rprintf (" b:");
      if (b->link[0])
	Rprintf ("l");
      if (b->link[1])
	Rprintf ("r");
      Rprintf ("\n");
      abort ();
    }
  if (a->link[0] != NULL)
    compare_trees (a->link[0], b->link[0]);
  if (a->rtag == PLUS)
    compare_trees (a->link[1], b->link[1]);
}

/* Simple stress test procedure for the AVL tree routines.  Does the
   following:

   * Generate a random number seed.  By default this is generated from
   the current time.  You can also pass a seed value on the command
   line if you want to test the same case.  The seed value is
   displayed.

   * Create a tree and insert the integers from 0 up to TREE_SIZE - 1
   into it, in random order.  Verify the tree structure after each
   insertion.
   
   * Remove each integer from the tree, in a different random order.
   After each deletion, verify the tree structure; also, make a copy
   of the tree into a new tree, verify the copy and compare it to the
   original, then destroy the copy.

   * Destroy the tree, increment the random seed value, and start over.

   If you make any modifications to the avl tree routines, then you
   might want to insert some calls to print_structure() at strategic
   places in order to be able to see what's really going on.  Also,
   memory debuggers like Checker or Purify are very handy. */
#define N_ITERATIONS 16
int
main (int argc, char **argv)
{
  int array[TREE_SIZE];
  int seed;
  int iteration;
  
  if (argc == 2)
    seed = atoi (argv[1]);
  else
    seed = time (0) * 257 % 32768;

  Rprintf ("Testing avltr...\n");

  for (iteration = 1; iteration <= N_ITERATIONS; iteration++)
    {
      avltr_tree *tree;
      int i;
      
      Rprintf ("Iteration %4d/%4d: seed=%5d", iteration, N_ITERATIONS, seed);
      fflush (stdout);
      
      srand (seed++);

      for (i = 0; i < TREE_SIZE; i++)
	array[i] = i;
      shuffle (array, TREE_SIZE);
      
      tree = avltr_create (compare_ints, NULL);
      for (i = 0; i < TREE_SIZE; i++)
	  avltr_force_insert (tree, (void *) (array[i]));
      verify_tree (tree);

      shuffle (array, TREE_SIZE);
      for (i = 0; i < TREE_SIZE; i++)
	{
	  avltr_tree *copy;

	  avltr_delete (tree, (void *) (array[i]));
	  verify_tree (tree);

	  copy = avltr_copy (tree, NULL);
	  verify_tree (copy);
	  if (tree->root.link[0] != NULL)
	    compare_trees (tree->root.link[0], copy->root.link[0]);
	  else if (copy->root.link[0] != NULL)
	    {
	      Rprintf (" Empty tree results in nonempty copy.\n");
	      abort ();
	    }
	  avltr_destroy (copy, NULL);

	  if (i % 128 == 0)
	    {
	      Rprintf (".");
	      fflush (stdout);
	    }
	}
      Printf(" good.\n", stdout);

      avltr_destroy (tree, NULL);
    }
  
  return 0;
}
#endif /* SELF_TEST */

/*
  Local variables:
  compile-command: "gcc -DSELF_TEST=1 -W -Wall -I. -o ./avltr-test avltr.c"
  End:
*/

