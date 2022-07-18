package gov.nih.mipav.view.renderer.WildMagic.AAM;


/**
 * CVisRect
 *
 * * AAM-API LICENSE  -  file: license.txt
 * 
 * This software is freely available for non-commercial use such as
 * research and education. Please see the full disclaimer below. 
 * 
 * All publications describing work using this software should cite 
 * the reference given below. 
 * 	
 * Copyright (c) 2000-2003 Mikkel B. Stegmann, mbs@imm.dtu.dk
 * 
 * 
 * IMM, Informatics & Mathematical Modelling
 * DTU, Technical University of Denmark
 * Richard Petersens Plads, Building 321
 * DK-2800 Lyngby, Denmark
 * 
 * http://www.imm.dtu.dk/~aam/
 * 
 * 
 * 
 * REFERENCES
 * 
 * Please use the reference below, when writing articles, reports etc. where 
 * the AAM-API has been used. A draft version the article is available from 
 * the homepage. 
 * 
 * I will be happy to receive pre- or reprints of such articles.
 * 
 * /Mikkel
 * 
 * 
 * -------------
 * M. B. Stegmann, B. K. Ersboll, R. Larsen, "FAME -- A Flexible Appearance 
 * Modelling Environment", IEEE Transactions on Medical Imaging, IEEE, 2003
 * (to appear)
 * -------------
 * 
 *
 * 
 * 3RD PART SOFTWARE
 * 
 * The software is partly based on the following libraries:
 * 
 * - The Microsoft(tm) Vision Software Developers Kit, VisSDK
 * - LAPACK
 * 
 *
 * DISCLAIMER
 * 
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the author be held liable for any damages arising from the
 * use of this software.
 * 
 * Permission is granted to anyone to use this software for any non-commercial 
 * purpose, and to alter it, subject to the following restrictions:
 * 
 * 1. The origin of this software must not be misrepresented; you must not claim
 *  that you wrote the original software. 
 *
 * 2. Altered source versions must be plainly marked as such, and must not be 
 *  misrepresented as being the original software.
 * 
 * 3. This notice may not be removed or altered from any source distribution.
 * 
 * --
 *
 * No guarantees of performance accompany this software, nor is any 
 * responsibility assumed on the part of the author or IMM. 
 * 
 * This software is provided by Mikkel B. Stegmann and IMM ``as is'' and any 
 * express or implied warranties, including, but not limited to, the implied 
 * warranties of merchantability and fitness for a particular purpose are 
 * disclaimed. In no event shall IMM or Mikkel B. Stegmann be liable for any 
 * direct, indirect, incidental, special, exemplary, or consequential damages
 * (including, but not limited to, procurement of substitute goods or services;
 * loss of use, data, or profits; or business interruption) however caused and 
 * on any theory of liability, whether in contract, strict liability, or tort 
 * (including negligence or otherwise) arising in any way out of the use of 
 * this software, even if advised of the possibility of such damage.
 * 
 * 
 * 
 *
 * $Revision: 1.4 $ 
 * $Date: 2003/04/23 14:49:15 $ 
 *  
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