package gov.nih.mipav.view.renderer.WildMagic.AAM;


/**
 * CVisRect
 *
 * Copyright © 2000 Microsoft Corporation, All Rights Reserved
 *
 * Being used by the CVisShape class.  
 *  
 * @author Ruida Cheng
 *
 */
public class CVisRect {
	
	public int left;
	public int top;
	public int right; 
	public int bottom;
	
	public CVisRect() {
		
	}
	
	public CVisRect(int l, int t, int r, int b) {
		left = l; 
		top = t; 
		right = r; 
		bottom = b; 
	}
	
	public CVisRect(POINT pt1, POINT pt2) { 
		left = pt1.x; 
		top = pt1.y; 
		right = pt2.x; 
		bottom = pt2.y; 
	}
	
	public CVisRect(POINT pt, SIZE size) { 
		left = pt.x; top = pt.y; 
		right = pt.x + size.x; 
		bottom = pt.y + size.y; 
	}
	
	public CVisRect(final CVisRect refrect) { 
		left = refrect.left; 
		top = refrect.top; 
		right = refrect.right; 
		bottom = refrect.bottom; 
	}
	
	// operator=
	public CVisRect assign(final CVisRect refrect) { 
		left = refrect.left; 
		top = refrect.top; 
		right = refrect.right; 
		bottom = refrect.bottom; 
		return this; 
	}
	
	public int Width() { 
		return right - left; 
	}
	
	public int Height() { 
		return bottom - top; 
	}
	
	
	public void InflateRect(int dx1, int dy1, int dx2, int dy2) { 
		left -= dx1; 
		top -= dy1; 
		right += dx2; 
		bottom += dy2; 
	}
	
	public boolean equals(final CVisRect refrect)  { 
		return ((left == refrect.left) && (top == refrect.top) && (right == refrect.right) && (bottom == refrect.bottom)); 
	}
	
	
	public boolean notequals(final CVisRect refrect) { 
		return !this.equals(refrect); 
	}
	
	public void NormalizeRect()
	{
		int w;
		if (left > right)
		{
			w = left;
			left = right;
			right = w;
		}
		if (top > bottom)
		{
			w = top;
			top = bottom;
			bottom = w;
		}
	}
	
}