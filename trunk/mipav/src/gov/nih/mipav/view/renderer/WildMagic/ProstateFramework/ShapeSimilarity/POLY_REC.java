package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity;

/**
 *  Ported to Java from C code from 
 *  http://www8.cs.umu.se/kurser/TDBAfl/VT06/algorithms/WEBSITE/IMPLEMEN/TURN/IMPLEMEN.HTM
 *  May only be used for non-commercial purposes.
 *  
 *  Implementation of "An Efficiently Computable Metric
 *  for Comparing Polygonal Shapes," by Arkin, Chew, Huttenlocher,
 *  Kedem, and Mitchel (undated).  This expands a little on the
 *  cited reference to achieve O(n) space and O(mn log n)
 *  run time.
 *
 *  This could be improved to O(min m,n) and O(mn log min m,n)
 *  by selecting the smallest of the 2 polys to create the initial
 *  event heap.  See init_events().
 *
 *  Variable names match the article.
 *
 *  Implementation (c) Eugene K. Ressler 91, 92  This source may be
 *  freely distributed and used for non-commercial purposes, so long 
 *  as this comment is attached to any code copied or derived from it.
 *  
 * @author Ruida Cheng
 *
 */
public class POLY_REC extends poly {
	
}