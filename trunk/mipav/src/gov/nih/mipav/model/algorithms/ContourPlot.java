package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

import java.util.*;

import java.awt.*;
import java.awt.geom.Point2D;

import java.util.Arrays;
import java.util.List;
import java.util.function.DoubleSupplier;
import java.util.function.IntSupplier;


/**
 * The Contours class provides methods to compute contour lines and contour bands
 * from a 2D regular grid of scalar values.
 * See {@link #computeContourLines(double[][], double, int)}
 * and {@link #computeContourBands(double[][], double, double, int, int)}
 * 
 * @author hageldave
 */

public class ContourPlot {
	/**
	 * License for JPlotter
	MIT License

	Copyright (c) 2019 David Hägele

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
	*/
	
	/**
	 * 
	License for ImagingKit
	MIT License (MIT)

	Copyright (c) 2017 David Hägele

	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is
	furnished to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
	SOFTWARE.
	*/

	
	
	
	
	/**
	 * Specification of a line segment which comprises vertex locations, colors, picking color, and thicknesses.
	 * @author hageldave
	 */
	public static class SegmentDetails implements Cloneable {
		protected static final DoubleSupplier[] PREDEFINED_THICKNESSES = new DoubleSupplier[]
				{()->0f, ()->1f, ()->2f, ()->3f, ()->4f};
		
		public Point2D p0;
		public Point2D p1;
		public IntSupplier color0;
		public IntSupplier color1;
		public DoubleSupplier thickness0 = PREDEFINED_THICKNESSES[1];
		public DoubleSupplier thickness1 = PREDEFINED_THICKNESSES[1];
		public int pickColor;

		public SegmentDetails(Point2D p0, Point2D p1) {
			this.p0 = p0;
			this.p1 = p1;
			this.color0 = this.color1 = ()->0xff555555;
		}
		
		/**
		 * Copies the specified {@link Point2D} (calls clone) and
		 * casts the copy to the class of the original.
		 * @param p point to copy
		 * @return the copied point
		 * 
		 * @param <T> type of Point2D
		 */
		@SuppressWarnings("unchecked")
		public  <T extends Point2D> T copy(T p){
			return (T) p.clone();
		}
		
		/**
		 * Returns a shallow copy of this segment with deep copied
		 * positions {@link #p0} and {@link #p1}.
		 * @return copy of this segment
		 */
		public SegmentDetails copy() {
			SegmentDetails clone = this.clone();
			clone.p0 = copy(clone.p0);
			clone.p1 = copy(clone.p1);
			return clone;
		}
		
		@Override
		public SegmentDetails clone() {
			try {
				return (SegmentDetails) super.clone();
			} catch (CloneNotSupportedException e) {
				// should never happen since cloneable
				throw new InternalError(e);
			}
		}
		
		/**
		 * Sets the picking color.
		 * When a non 0 transparent color is specified its alpha channel will be set to 0xff to make it opaque.
		 * @param pickID picking color of the segment (see {@link Lines} for details)
		 * @return this for chaining
		 */
		public SegmentDetails setPickColor(int pickID){
			if(pickID != 0)
				pickID = pickID | 0xff000000;
			this.pickColor = pickID;
			return this;
		}
		
		/**
		 * Sets the color at the starting point of the segment
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor0(IntSupplier color){
			this.color0 = color;
			return this;
		}
		
		/**
		 * Sets the color at the end point of the segment
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor1(IntSupplier color){
			this.color1 = color;
			return this;
		}
		
		/**
		 * Sets the color of the segment (start and end point)
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor(IntSupplier color){
			this.color0 = this.color1 = color;
			return this;
		}
		
		/**
		 * Sets the color at the starting point of the segment
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor0(int color){
			return setColor0(()->color);
		}
		
		/**
		 * Sets the color at the starting point of the segment
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor1(int color){
			return setColor1(()->color);
		}
		
		/**
		 * Sets the color of the segment (start and end point)
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public SegmentDetails setColor(int color){
			return setColor(()->color);
		}
		
		/**
		 * Sets the color at the starting point of the segment
		 * @param color for starting point
		 * @return this for chaining
		 */
		public SegmentDetails setColor0(Color color){
			return setColor0(color.getRGB());
		}
		
		/**
		 * Sets the color at the end point of the segment
		 * @param color for end point
		 * @return this for chaining
		 */
		public SegmentDetails setColor1(Color color){
			return setColor1(color.getRGB());
		}
		
		/**
		 * Sets the color of the segment (start and end point)
		 * @param color of the segment
		 * @return this for chaining
		 */
		public SegmentDetails setColor(Color color){
			return setColor(color.getRGB());
		}
		
		public SegmentDetails setThickness(double t){
			return setThickness(sup4thick(t));
		}
		
		public SegmentDetails setThickness(DoubleSupplier t){
			this.thickness0 = this.thickness1 = t;
			return this;
		}
		
		public SegmentDetails setThickness(double t0, double t1){
			return setThickness(sup4thick(t0), sup4thick(t1));
		}
		
		public SegmentDetails setThickness(DoubleSupplier t0, DoubleSupplier t1){
			this.thickness0 = t0;
			this.thickness1 = t1;
			return this;
		}
		
		protected static DoubleSupplier sup4thick(double t){
			if( t == ((int)t) && t >= 0 && t < PREDEFINED_THICKNESSES.length){
				return PREDEFINED_THICKNESSES[(int)t];
			}
			return ()->t;
		}
		
	}


	
	/**
	 * Specification of a triangle which comprises vertex locations, colors and picking color.
	 * @author hageldave
	 */
	public static class TriangleDetails implements Cloneable {
		public Point2D p0,p1,p2;
		public IntSupplier c0,c1,c2;
		public int pickColor;
		
		public TriangleDetails(
				Point2D p0,
				Point2D p1,
				Point2D p2)
		{
			this.p0 = p0;
			this.p1 = p1;
			this.p2 = p2;
			this.c0 = c1 = c2 = ()->0xffaaaaaa;
		}
		
		public TriangleDetails(
				double x0, double y0, 
				double x1, double y1,
				double x2, double y2)
		{
			this(new Point2D.Float((float)x0, (float)y0), new Point2D.Float((float)x1, (float)y1), new Point2D.Float((float)x2, (float)y2));
		}
		
		/**
		 * Returns a shallow copy of this triangle.
		 * @return copy of this triangle
		 */
		public TriangleDetails copy() {
			return clone();
		}
		
		public TriangleDetails clone() {
			try {
	            return (TriangleDetails) super.clone();
	        } catch (CloneNotSupportedException e) {
	            // this shouldn't happen, since we are Cloneable
	            throw new InternalError(e);
	        }
		}
		
		/**
		 * Sets the picking color.
		 * When a non 0 transparent color is specified its alpha channel will be set to 0xff to make it opaque.
		 * @param pickID picking color of the triangle (see {@link Triangles} for details)
		 * @return this for chaining
		 */
		public TriangleDetails setPickColor(int pickID){
			if(pickID != 0)
				pickID = pickID | 0xff000000;
			this.pickColor = pickID;
			return this;
		}
		
		/**
		 * Sets the color for vertex 0
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor0(IntSupplier color){
			this.c0 = color;
			return this;
		}
		
		/**
		 * Sets the color for vertex 1
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor1(IntSupplier color){
			this.c1 = color;
			return this;
		}
		
		/**
		 * Sets the color for vertex 2
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor2(IntSupplier color){
			this.c2 = color;
			return this;
		}
		
		/**
		 * Sets the color of the triangle (all vertices)
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor(IntSupplier color){
			this.c0 = this.c1 = this.c2 = color;
			return this;
		}
		
		/**
		 * Sets the color of vertex 0
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor0(int color){
			return setColor0(()->color);
		}
		
		/**
		 * Sets the color of vertex 1
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor1(int color){
			return setColor1(()->color);
		}
		
		/**
		 * Sets the color of vertex 2
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor2(int color){
			return setColor2(()->color);
		}
		
		/**
		 * Sets the color of the triangle (all vertices)
		 * @param color integer packed ARGB color value (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public TriangleDetails setColor(int color){
			return setColor(()->color);
		}
		
		/**
		 * Sets the color of vertex 0
		 * @param color of v0
		 * @return this for chaining
		 */
		public TriangleDetails setColor0(Color color){
			return setColor0(color.getRGB());
		}
		
		/**
		 * Sets the color of vertex 1
		 * @param color of v1
		 * @return this for chaining
		 */
		public TriangleDetails setColor1(Color color){
			return setColor1(color.getRGB());
		}
		
		/**
		 * Sets the color of vertex 2
		 * @param color of v2
		 * @return this for chaining
		 */
		public TriangleDetails setColor2(Color color){
			return setColor2(color.getRGB());
		}
		
		/**
		 * Sets the color the triangle (all vertices)
		 * @param color of the triangle
		 * @return this for chaining
		 */
		public TriangleDetails setColor(Color color){
			return setColor(color.getRGB());
		}
	}
	
	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return blue component(8bit) of specified color.
	 * @see #a(int)
	 * @see #r(int)
	 * @see #g(int)
	 * @see #argb(int, int, int, int)
	 * @see #rgb(int, int, int)
	 * @since 1.0
	 */
	public static final int b(final int color){
		return (color) & 0xff;
	}

	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return green component(8bit) of specified color.
	 * @see #a(int)
	 * @see #r(int)
	 * @see #b(int)
	 * @see #argb(int, int, int, int)
	 * @see #rgb(int, int, int)
	 * @since 1.0
	 */
	public static final int g(final int color){
		return (color >> 8) & 0xff;
	}

	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return red component(8bit) of specified color.
	 * @see #a(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @see #argb(int, int, int, int)
	 * @see #rgb(int, int, int)
	 * @since 1.0
	 */
	public static final int r(final int color){
		return (color >> 16) & 0xff;
	}

	/**
	 * @param color ARGB(32bit) value
	 * @return alpha component(8bit) of specified color.
	 * @see #r(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @see #argb(int, int, int, int)
	 * @see #rgb(int, int, int)
	 * @since 1.0
	 */
	public static final int a(final int color){
		return (color >> 24) & 0xff;
	}


	
	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return normalized blue component of specified color <br>
	 * (value in [0.0 .. 1.0]).
	 * @see #b()
	 * @see #r_normalized(int)
	 * @see #g_normalized(int)
	 * @see #a_normalized(int)
	 * @since 1.2
	 */
	public static final double b_normalized(final int color){
		return b(color)/255.0;
	}

	
	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return normalized green component of specified color <br>
	 * (value in [0.0 .. 1.0]).
	 * @see #g()
	 * @see #r_normalized(int)
	 * @see #b_normalized(int)
	 * @see #a_normalized(int)
	 * @since 1.2
	 */
	public static final double g_normalized(final int color){
		return g(color)/255.0;
	}

	/**
	 * @param color ARGB(32bit) or RGB(24bit) value
	 * @return normalized red component of specified color <br>
	 * (value in [0.0 .. 1.0]).
	 * @see #r()
	 * @see #b_normalized(int)
	 * @see #g_normalized(int)
	 * @see #a_normalized(int)
	 * @since 1.2
	 */
	public static final double r_normalized(final int color){
		return r(color)/255.0;
	}

	/**
	 * @param color ARGB(32bit) value
	 * @return normalized alpha component of specified color <br>
	 * (value in [0.0 .. 1.0]).
	 * @see #a()
	 * @see #r_normalized(int)
	 * @see #g_normalized(int)
	 * @see #a_normalized(int)
	 * @since 1.2
	 */
	public static final double a_normalized(final int color){
		return a(color)/255.0;
	}
	
	/**
	 * Packs 8bit ARGB color components into a single 32bit integer value.
	 * Components larger than 8bit are NOT truncated and will result in a
	 * broken, malformed value.
	 * @param a alpha
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #argb(int, int, int, int)
	 * @see #argb_bounded(int, int, int, int)
	 * @see #rgb_bounded(int, int, int)
	 * @see #rgb(int, int, int)
	 * @see #rgb_fast(int, int, int)
	 * @see #a(int)
	 * @see #r(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @since 1.0
	 */
	public static final int argb_fast(final int a, final int r, final int g, final int b){
		return (a<<24)|(r<<16)|(g<<8)|b;
	}


	
	/**
	 * Packs 8bit ARGB color components into a single 32bit integer value.
	 * Components are clamped to [0,255].
	 * @param a alpha
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #argb(int, int, int, int)
	 * @see #argb_fast(int, int, int, int)
	 * @see #rgb_bounded(int, int, int)
	 * @see #rgb(int, int, int)
	 * @see #rgb_fast(int, int, int)
	 * @see #a(int)
	 * @see #r(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @since 1.0
	 */
	public static final int argb_bounded(final int a, final int r, final int g, final int b){
		return argb_fast(
				a > 255 ? 255: a < 0 ? 0:a,
				r > 255 ? 255: r < 0 ? 0:r,
				g > 255 ? 255: g < 0 ? 0:g,
				b > 255 ? 255: b < 0 ? 0:b);
	}


	
	/**
	 * Packs normalized ARGB color components (values in [0.0 .. 1.0]) into a
	 * single 32bit integer value.
	 * Component values less than 0 or greater than 1 are clamped to fit the range.
	 * @param a alpha
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #rgb_fromNormalized(double, double, double)
	 * @see #argb(int, int, int, int)
	 * @see #a_normalized(int)
	 * @see #r_normalized(int)
	 * @see #g_normalized(int)
	 * @see #b_normalized(int)
	 * @since 1.2
	 */
	public static final int argb_fromNormalized(final double a, final double r, final double g, final double b){
		return argb_bounded((int)Math.round(a*0xff), (int)Math.round(r*0xff), (int)Math.round(g*0xff), (int)Math.round(b*0xff));
	}


	
	/**
	 * Linearly interpolates between the two specified colors.<br>
	 * c = c1*(1-m) + c2*m
	 * @param c1 integer packed ARGB color value 
	 * @param c2 integer packed ARGB color value, e.g. 0xff00ff00 for opaque green
	 * @param m in [0,1]
	 * @return interpolated color
	 */
	public static int interpolateColor(int c1, int c2, double m){
		double r1 = r_normalized(c1);
		double g1 = g_normalized(c1);
		double b1 = b_normalized(c1);
		double a1 = a_normalized(c1);
		
		double r2 = r_normalized(c2);
		double g2 = g_normalized(c2);
		double b2 = b_normalized(c2);
		double a2 = a_normalized(c2);
		
		return argb_fromNormalized(
				a1*(1-m)+a2*m,
				r1*(1-m)+r2*m,
				g1*(1-m)+g2*m,
				b1*(1-m)+b2*m
		);
	}
	
	/**
	 * Computes the contour lines from the grid samples of a bivariate function z(x,y).
	 * The resulting line segments are solutions to the equation { (x,y) | z(x,y)=iso }
	 * within the grid.
	 * <p>
	 * <b>About indices</b>
	 * For cartesian or rectilinear grids, x varies with the inner index of the 2D array, y with the outer index:<br>
	 * X<sub>ij</sub>=X[i][j]=X[?][j].<br>
	 * Y<sub>ij</sub>=Y[i][j]=Y[i][?]
	 * 
	 * @param X x-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) ) 
	 * @param Y y-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) )
	 * @param Z z-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) )
	 * @param isoValue the iso value for which the contour (iso) lines should be computed
	 * @param color integer packed ARGB color value the returned line segments should have, e.g. 0xff00ff00 for opaque green.
	 * @return list of line segments that form the contour lines. There is no particular order so subsequent segments are not 
	 * necessarily adjacent.
	 */
	public static List<SegmentDetails> computeContourLines(double[][] X, double[][] Y, double[][] Z, double isoValue, int color){
		List<SegmentDetails> contourLines = computeContourLines(Z, isoValue, color);
		for(SegmentDetails segment:contourLines){
			for(Point2D p : Arrays.asList(segment.p0,segment.p1)){
				int j = (int)p.getX();
				int i = (int)p.getY();
				double mi = p.getX()-j;
				double mj = p.getY()-i;
				double xcoord = X[i][j];
				if(mi > 1e-6){
					xcoord = X[i][j]+mi*(X[i][j+1]-X[i][j]);
				}
				double ycoord = Y[i][j];
				if(mj > 1e-6){
					ycoord = Y[i][j]+mj*(Y[i+1][j]-Y[i][j]);
				}
				p.setLocation(xcoord, ycoord);
			}
		}
		return contourLines;
	}
	
	/**
	 * Computes the contour bands from the grid samples of a bivariate function z(x,y).
	 * The resulting triangles are solutions to the equation { (x,y) | iso1 &lt; z(x,y) &lt; iso2 }
	 * within the grid.
	 * <p>
	 * <b>About indices</b>
	 * For cartesian or rectilinear grids, x varies with the inner index of the 2D array, y with the outer index:<br>
	 * X<sub>ij</sub>=X[i][j]=X[?][j].<br>
	 * Y<sub>ij</sub>=Y[i][j]=Y[i][?]
	 * 
	 * @param X x-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) )
	 * @param Y y-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) )
	 * @param Z z-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (X<sub>ij</sub>,Y<sub>ij</sub>,Z<sub>ij</sub>) )
	 * @param isoValue1 the lower bound for values of the iso bands
	 * @param isoValue2 the upper bound for values of the iso bands
	 * @param c1 color for the isoValue1
	 * @param c2 color for the isoValue2, values in between iso1 and iso2 will have their color linearly interpolated
	 * @return list of triangles that form the iso bands. The order of triangles does NOT imply any adjacency between them.
	 */
	public static List<TriangleDetails> computeContourBands(double[][] X, double[][] Y, double[][] Z, double isoValue1, double isoValue2, int c1, int c2){
		List<TriangleDetails> contourBands = computeContourBands(Z, isoValue1, isoValue2, c1, c2);
		double[] xCoords = new double[3];
		double[] yCoords = new double[3];
		for(TriangleDetails tri:contourBands){
			xCoords[0]=tri.p0.getX(); xCoords[1]=tri.p1.getX(); xCoords[2]=tri.p2.getX();
			yCoords[0]=tri.p0.getY(); yCoords[1]=tri.p1.getY(); yCoords[2]=tri.p2.getY();
			for(int t=0; t<3; t++){
				int j = (int)xCoords[t];
				int i = (int)yCoords[t];
				double mi = xCoords[t]-j;
				double mj = yCoords[t]-i;
				double xcoord = X[i][j];
				if(mi > 1e-6){
					xcoord = X[i][j]+mi*(X[i][j+1]-X[i][j]);
				}
				double ycoord = Y[i][j];
				if(mj > 1e-6){
					ycoord = Y[i][j]+mj*(Y[i+1][j]-Y[i][j]);
				}
				xCoords[t] = xcoord;
				yCoords[t] = ycoord;
			}
			tri.p0.setLocation(xCoords[0],yCoords[0]);
			tri.p1.setLocation(xCoords[1],yCoords[1]);
			tri.p2.setLocation(xCoords[2],yCoords[2]);
		}
		return contourBands;
	}

	/**
	 * Computes the contour lines from the grid samples of a bivariate function z(x,y)<br>
	 * with implicit integer valued (x,y) = (i,j).<br>
	 * The resulting line segments are solutions to the equation { (x,y) | z(x,y)=iso }
	 * within the grid.
	 * <p>
	 * The lines are computed using the Meandering Triangles algorithm which divides each
	 * square cell of the grid into 2 triangle cells first before computing isovalue intersections
	 * on the triangle sides.
	 * The more well known Marching Squares algorithm has significantly more cell cases to check, 
	 * which is why Meandering Triangles was preferred here.
	 * 
	 * @param uniformGridSamples z-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (i,j,Z<sub>ij</sub>) )
	 * @param isoValue the iso value for which the contour (iso) lines should be computed
	 * @param color integer packed ARGB color value the returned line segments should have, e.g. 0xff00ff00 for opaque green.
	 * @return list of line segments that form the contour lines. There is no particular order so subsequent segments are not 
	 * necessarily adjacent.
	 */
	public static List<SegmentDetails> computeContourLines(double[][] uniformGridSamples, double isoValue, int color){
		int height = uniformGridSamples.length;
		int width = uniformGridSamples[0].length;
		double[][] f = uniformGridSamples; // shorthand
		// mark nodes that have a greater value than the iso value
		boolean[][] greaterThanIso = new boolean[height][width];
		for(int i=0; i<height; i++){
			for(int j=0; j<width; j++){
				greaterThanIso[i][j] = f[i][j] > isoValue;
			}
		}
		LinkedList<SegmentDetails> cntrLineSegments = new LinkedList<>();
		IntSupplier color_ = ()->color;
		/* 
		 * go through all cells, determine cell type and add corresponding line segments to list
		 */
		for(int i=0; i<height-1; i++){
			for(int j=0; j<width-1; j++){
				for(int t=0; t<2;t++){
					int celltype;
					double tx0,ty0,tx1,ty1,tx2,ty2;
					double v0, v1, v2;
					if(t == 0){
						// lt, rt, lb
						tx0=j+0; ty0=i+0;
						tx1=j+1; ty1=i+0;
						tx2=j+0; ty2=i+1;
						celltype = celltype(
								greaterThanIso[i][j], 
								greaterThanIso[i][j+1], 
								greaterThanIso[i+1][j]);
						v0 = f[i][j];
						v1 = f[i][j+1];
						v2 = f[i+1][j];
					} else {
						// rb, lb, rt
						tx0=j+1; ty0=i+1;
						tx1=j+0; ty1=i+1;
						tx2=j+1; ty2=i+0;
						celltype = celltype(
								greaterThanIso[i+1][j+1], 
								greaterThanIso[i+1][j], 
								greaterThanIso[i][j+1]);
						v0 = f[i+1][j+1];
						v1 = f[i+1][j];
						v2 = f[i][j+1];
					}
					switch (celltype) {
					// non intersecting celltypes
					case 0b000: // fall through
					case 0b111: // no intersection of isoline in this cell
						break;
					case 0b100:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v1, v0, isoValue);
						m1 = 1-interpolateToValue(v2, v0, isoValue);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					case 0b010:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v1, isoValue);
						m1 = 1-interpolateToValue(v2, v1, isoValue);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					case 0b001:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v2, isoValue);
						m1 = 1-interpolateToValue(v1, v2, isoValue);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					case 0b011:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v0, v1, isoValue);
						m1 = interpolateToValue(v0, v2, isoValue);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					case 0b101:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v1, v0, isoValue);
						m1 = interpolateToValue(v1, v2, isoValue);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					case 0b110:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v2, v0, isoValue);
						m1 = interpolateToValue(v2, v1, isoValue);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						cntrLineSegments.add(new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
						break;
					}
					default:
						break;
					}
				}
			}
		}
		return cntrLineSegments;
	}
	
	/**
	 * Computes the contour bands from the grid samples of a bivariate function z(x,y)<br>
	 * with implicit integer valued (x,y) = (i,j).<br>
	 * The resulting triangles are solutions to the equation { (x,y) | iso1 &lt; z(x,y) &lt; iso2 }
	 * within the grid.
	 * <p>
	 * The triangles are computed using the Meandering Triangles algorithm which divides each
	 * square cell of the grid into 2 triangle cells first before computing isovalue intersections
	 * on the triangle sides.
	 * The more well known Marching Squares algorithm has significantly more cell cases to check, 
	 * which is why Meandering Triangles was preferred here.
	 * 
	 * @param uniformGridSamples z-coordinates of the grid points ( (x,y,z)<sub>ij</sub> = (i,j,Z<sub>ij</sub>) )
	 * @param isoValue1 the lower bound for values of the iso bands
	 * @param isoValue2 the upper bound for values of the iso bands
	 * @param c1 color for the isoValue1
	 * @param c2 color for the isoValue2, values in between iso1 and iso2 will have their color linearly interpolated
	 * @return list of triangles that form the iso bands. The order of triangles does NOT imply any adjacency between them.
	 */
	public static List<TriangleDetails> computeContourBands(double[][] uniformGridSamples, double isoValue1, double isoValue2, int c1, int c2){
		if(isoValue1 > isoValue2){
			// swap
			return computeContourBands(uniformGridSamples, isoValue2, isoValue1, c2, c1);
		}
		int height = uniformGridSamples.length;
		int width = uniformGridSamples[0].length;
		double[][] f = uniformGridSamples; // shorthand
		// mark nodes that have a greater value than the iso value
		boolean[][] greaterThanIso1 = new boolean[height][width];
		boolean[][] greaterThanIso2 = new boolean[height][width];
		for(int i=0; i<height; i++){
			for(int j=0; j<width; j++){
				greaterThanIso1[i][j] = f[i][j] > isoValue1;
				greaterThanIso2[i][j] = f[i][j] > isoValue2;
			}
		}
		LinkedList<TriangleDetails> tris = new LinkedList<>();
		IntSupplier c1_ = ()->c1;
		IntSupplier c2_ = ()->c2;
		/* 
		 * go through all cells, determine cell type and add corresponding line segments to list
		 */
		for(int i=0; i<height-1; i++){
			for(int j=0; j<width-1; j++){
				for(int t=0; t<2;t++){
					int celltype;
					double tx0,ty0,tx1,ty1,tx2,ty2;
					double v0, v1, v2;
					if(t == 0){
						// lt, rt, lb
						tx0=j+0; ty0=i+0;
						tx1=j+1; ty1=i+0;
						tx2=j+0; ty2=i+1;
						celltype = celltype(
								greaterThanIso1[i][j], 
								greaterThanIso1[i][j+1], 
								greaterThanIso1[i+1][j],
								greaterThanIso2[i][j], 
								greaterThanIso2[i][j+1], 
								greaterThanIso2[i+1][j]);
						v0 = f[i][j];
						v1 = f[i][j+1];
						v2 = f[i+1][j];
					} else {
						// rb, lb, rt
						tx0=j+1; ty0=i+1;
						tx1=j+0; ty1=i+1;
						tx2=j+1; ty2=i+0;
						celltype = celltype(
								greaterThanIso1[i+1][j+1], 
								greaterThanIso1[i+1][j], 
								greaterThanIso1[i][j+1],
								greaterThanIso2[i+1][j+1], 
								greaterThanIso2[i+1][j], 
								greaterThanIso2[i][j+1]);
						v0 = f[i+1][j+1];
						v1 = f[i+1][j];
						v2 = f[i][j+1];
					}
					switch (celltype) {
					// non intersecting celltypes
					case 0x000: // fall through
					case 0x222: // no intersection of isoline in this cell
						break;
					case 0x111:{
						tris.add(new TriangleDetails(tx0,ty0, tx1,ty1, tx2,ty2)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
						);
						break;
					}
					// one corner cases
					case 0x100:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v1, v0, isoValue1);
						m1 = 1-interpolateToValue(v2, v0, isoValue1);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						tris.add(new TriangleDetails(tx0,ty0, x0,y0, x1,y1)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0))) 
								.setColor1(c1_) 
								.setColor2(c1_)
						);
						break;
					}
					case 0x122:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v0, v1, isoValue2);
						m1 = interpolateToValue(v0, v2, isoValue2);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						tris.add(new TriangleDetails(tx0,ty0, x0,y0, x1,y1)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(c2_) 
								.setColor2(c2_));
						break;
					}
					case 0x010:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v1, isoValue1);
						m1 = 1-interpolateToValue(v2, v1, isoValue1);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						tris.add(new TriangleDetails(x0,y0, tx1,ty1, x1,y1)
								.setColor0(c1_) 
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c1_));
						break;
					}
					case 0x212:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v1, v0, isoValue2);
						m1 = interpolateToValue(v1, v2, isoValue2);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						tris.add(new TriangleDetails(x0,y0, tx1,ty1, x1,y1)
								.setColor0(c2_) 
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1))) 
								.setColor2(c2_));
						break;
					}
					case 0x001:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v2, isoValue1);
						m1 = 1-interpolateToValue(v1, v2, isoValue1);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, tx2,ty2)
								.setColor0(c1_)  
								.setColor1(c1_)
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2))));
						break;
					}
					case 0x221:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v2, v0, isoValue2);
						m1 = interpolateToValue(v2, v1, isoValue2);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, tx2,ty2)
								.setColor0(c2_)  
								.setColor1(c2_)
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2))));
						break;
					}
					
					
					// two corner cases
					case 0x011:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v0, v1, isoValue1);
						m1 = interpolateToValue(v0, v2, isoValue1);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						tris.add(new TriangleDetails(tx1,ty1, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						tris.add(new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
								.setColor0(c1_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						break;
					}
					case 0x211:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v1, v0, isoValue2);
						m1 = 1-interpolateToValue(v2, v0, isoValue2);
						x0 = tx0+m0*(tx1-tx0);
						y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx2-tx0);
						y1 = ty0+m1*(ty2-ty0);
						tris.add(new TriangleDetails(tx1,ty1, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						tris.add(new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
								.setColor0(c2_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						break;
					}
					case 0x101:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v1, v0, isoValue1);
						m1 = interpolateToValue(v1, v2, isoValue1);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						tris.add(new TriangleDetails(tx0,ty0, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						tris.add(new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
								.setColor0(c1_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						break;
					}
					case 0x121:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v1, isoValue2);
						m1 = 1-interpolateToValue(v2, v1, isoValue2);
						x0 = tx1+m0*(tx0-tx1);
						y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx2-tx1);
						y1 = ty1+m1*(ty2-ty1);
						tris.add(new TriangleDetails(tx0,ty0, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						tris.add(new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
								.setColor0(c2_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						break;
					}
					case 0x110:{
						double x0,y0,x1,y1, m0, m1;
						m0 = interpolateToValue(v2, v0, isoValue1);
						m1 = interpolateToValue(v2, v1, isoValue1);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						tris.add(new TriangleDetails(tx0,ty0, tx1,ty1, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c1_)
						);
						tris.add(new TriangleDetails(x1,y1, tx1,ty1, x0,y0)
								.setColor0(c1_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c1_)
						);
						break;
					}
					case 0x112:{
						double x0,y0,x1,y1, m0, m1;
						m0 = 1-interpolateToValue(v0, v2, isoValue2);
						m1 = 1-interpolateToValue(v1, v2, isoValue2);
						x0 = tx2+m0*(tx0-tx2);
						y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx1-tx2);
						y1 = ty2+m1*(ty1-ty2);
						tris.add(new TriangleDetails(tx0,ty0, tx1,ty1, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c2_)
						);
						tris.add(new TriangleDetails(x1,y1, tx1,ty1, x0,y0)
								.setColor0(c2_)
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c2_)
						);
						break;
					}
					// entirely in between vertices
					case 0x200:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v1, v0, isoValue1);
						m1 = interpolateToValue(v1, v0, isoValue2);
						m2 = interpolateToValue(v2, v0, isoValue1);
						m3 = interpolateToValue(v2, v0, isoValue2);
						x0 = tx1+m0*(tx0-tx1); y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx0-tx1); y1 = ty1+m1*(ty0-ty1);
						x2 = tx2+m2*(tx0-tx2); y2 = ty2+m2*(ty0-ty2);
						x3 = tx2+m3*(tx0-tx2); y3 = ty2+m3*(ty0-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					case 0x020:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v0, v1, isoValue1);
						m1 = interpolateToValue(v0, v1, isoValue2);
						m2 = interpolateToValue(v2, v1, isoValue1);
						m3 = interpolateToValue(v2, v1, isoValue2);
						x0 = tx0+m0*(tx1-tx0); y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx1-tx0); y1 = ty0+m1*(ty1-ty0);
						x2 = tx2+m2*(tx1-tx2); y2 = ty2+m2*(ty1-ty2);
						x3 = tx2+m3*(tx1-tx2); y3 = ty2+m3*(ty1-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					case 0x002:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v0, v2, isoValue1);
						m1 = interpolateToValue(v0, v2, isoValue2);
						m2 = interpolateToValue(v1, v2, isoValue1);
						m3 = interpolateToValue(v1, v2, isoValue2);
						x0 = tx0+m0*(tx2-tx0); y0 = ty0+m0*(ty2-ty0);
						x1 = tx0+m1*(tx2-tx0); y1 = ty0+m1*(ty2-ty0);
						x2 = tx1+m2*(tx2-tx1); y2 = ty1+m2*(ty2-ty1);
						x3 = tx1+m3*(tx2-tx1); y3 = ty1+m3*(ty2-ty1);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					case 0x220:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v2, v0, isoValue1);
						m1 = interpolateToValue(v2, v0, isoValue2);
						m2 = interpolateToValue(v2, v1, isoValue1);
						m3 = interpolateToValue(v2, v1, isoValue2);
						x0 = tx2-m0*(tx2-tx0); y0 = ty2-m0*(ty2-ty0);
						x1 = tx2-m1*(tx2-tx0); y1 = ty2-m1*(ty2-ty0);
						x2 = tx2-m2*(tx2-tx1); y2 = ty2-m2*(ty2-ty1);
						x3 = tx2-m3*(tx2-tx1); y3 = ty2-m3*(ty2-ty1);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					case 0x202:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v1, v0, isoValue1);
						m1 = interpolateToValue(v1, v0, isoValue2);
						m2 = interpolateToValue(v1, v2, isoValue1);
						m3 = interpolateToValue(v1, v2, isoValue2);
						x0 = tx1-m0*(tx1-tx0); y0 = ty1-m0*(ty1-ty0);
						x1 = tx1-m1*(tx1-tx0); y1 = ty1-m1*(ty1-ty0);
						x2 = tx1-m2*(tx1-tx2); y2 = ty1-m2*(ty1-ty2);
						x3 = tx1-m3*(tx1-tx2); y3 = ty1-m3*(ty1-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					case 0x022:{
						double x0,y0,x1,y1,x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v0, v1, isoValue1);
						m1 = interpolateToValue(v0, v1, isoValue2);
						m2 = interpolateToValue(v0, v2, isoValue1);
						m3 = interpolateToValue(v0, v2, isoValue2);
						x0 = tx0-m0*(tx0-tx1); y0 = ty0-m0*(ty0-ty1);
						x1 = tx0-m1*(tx0-tx1); y1 = ty0-m1*(ty0-ty1);
						x2 = tx0-m2*(tx0-tx2); y2 = ty0-m2*(ty0-ty2);
						x3 = tx0-m3*(tx0-tx2); y3 = ty0-m3*(ty0-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						break;
					}
					// mixed cases (pentagons)
					case 0x012:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v0, v2, isoValue1);
						m1 = interpolateToValue(v0, v2, isoValue2);
						m2 = interpolateToValue(v0, v1, isoValue1);
						m3 = interpolateToValue(v1, v2, isoValue2);
						x0 = tx0+m0*(tx2-tx0); y0 = ty0+m0*(ty2-ty0);
						x1 = tx0+m1*(tx2-tx0); y1 = ty0+m1*(ty2-ty0);
						x2 = tx0+m2*(tx1-tx0); y2 = ty0+m2*(ty1-ty0);
						x3 = tx1+m3*(tx2-tx1); y3 = ty1+m3*(ty2-ty1);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx1,ty1)
								.setColor0(c2_) 
								.setColor1(c1_)
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
						);
						break;
					}
					case 0x102:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v1, v2, isoValue1);
						m1 = interpolateToValue(v1, v2, isoValue2);
						m2 = interpolateToValue(v1, v0, isoValue1);
						m3 = interpolateToValue(v0, v2, isoValue2);
						x0 = tx1+m0*(tx2-tx1); y0 = ty1+m0*(ty2-ty1);
						x1 = tx1+m1*(tx2-tx1); y1 = ty1+m1*(ty2-ty1);
						x2 = tx1+m2*(tx0-tx1); y2 = ty1+m2*(ty0-ty1);
						x3 = tx0+m3*(tx2-tx0); y3 = ty0+m3*(ty2-ty0);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx0,ty0)
								.setColor0(c2_)
								.setColor1(c1_) 
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0))) 
						);
						break;
					}
					case 0x120:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v2, v1, isoValue1);
						m1 = interpolateToValue(v2, v1, isoValue2);
						m2 = interpolateToValue(v2, v0, isoValue1);
						m3 = interpolateToValue(v0, v1, isoValue2);
						x0 = tx2+m0*(tx1-tx2); y0 = ty2+m0*(ty1-ty2);
						x1 = tx2+m1*(tx1-tx2); y1 = ty2+m1*(ty1-ty2);
						x2 = tx2+m2*(tx0-tx2); y2 = ty2+m2*(ty0-ty2);
						x3 = tx0+m3*(tx1-tx0); y3 = ty0+m3*(ty1-ty0);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx0,ty0)
								.setColor0(c2_) 
								.setColor1(c1_) 
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0))) 
						);
						break;
					}
					case 0x210:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v2, v0, isoValue1);
						m1 = interpolateToValue(v2, v0, isoValue2);
						m2 = interpolateToValue(v2, v1, isoValue1);
						m3 = interpolateToValue(v1, v0, isoValue2);
						x0 = tx2+m0*(tx0-tx2); y0 = ty2+m0*(ty0-ty2);
						x1 = tx2+m1*(tx0-tx2); y1 = ty2+m1*(ty0-ty2);
						x2 = tx2+m2*(tx1-tx2); y2 = ty2+m2*(ty1-ty2);
						x3 = tx1+m3*(tx0-tx1); y3 = ty1+m3*(ty0-ty1);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx1,ty1)
								.setColor0(c2_) 
								.setColor1(c1_) 
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
						);
						break;
					}
					case 0x201:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v1, v0, isoValue1);
						m1 = interpolateToValue(v1, v0, isoValue2);
						m2 = interpolateToValue(v1, v2, isoValue1);
						m3 = interpolateToValue(v2, v0, isoValue2);
						x0 = tx1+m0*(tx0-tx1); y0 = ty1+m0*(ty0-ty1);
						x1 = tx1+m1*(tx0-tx1); y1 = ty1+m1*(ty0-ty1);
						x2 = tx1+m2*(tx2-tx1); y2 = ty1+m2*(ty2-ty1);
						x3 = tx2+m3*(tx0-tx2); y3 = ty2+m3*(ty0-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx2,ty2)
								.setColor0(c2_) 
								.setColor1(c1_)
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
						);
						break;
					}
					case 0x021:{
						double x0,y0,x1,y1, x2,y2,x3,y3, m0,m1,m2,m3;
						m0 = interpolateToValue(v0, v1, isoValue1);
						m1 = interpolateToValue(v0, v1, isoValue2);
						m2 = interpolateToValue(v0, v2, isoValue1);
						m3 = interpolateToValue(v2, v1, isoValue2);
						x0 = tx0+m0*(tx1-tx0); y0 = ty0+m0*(ty1-ty0);
						x1 = tx0+m1*(tx1-tx0); y1 = ty0+m1*(ty1-ty0);
						x2 = tx0+m2*(tx2-tx0); y2 = ty0+m2*(ty2-ty0);
						x3 = tx2+m3*(tx1-tx2); y3 = ty2+m3*(ty1-ty2);
						tris.add(new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(new TriangleDetails(x3,y3, x2,y2, tx2,ty2)
								.setColor0(c2_) 
								.setColor1(c1_) 
								.setColor2(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
						);
						break;
					}
					
					default:
//						throw new RuntimeException(Integer.toHexString(celltype));
//						break;
					}
				}
			}
		}
		return tris;
	}

	static int celltype(boolean v1, boolean v2, boolean v3){
		int type = 0;
		type = (type<<1) | (v1 ? 1:0);
		type = (type<<1) | (v2 ? 1:0);
		type = (type<<1) | (v3 ? 1:0);
		return type;
	}
	
	static int celltype(
			boolean v11, boolean v21, boolean v31,
			boolean v12, boolean v22, boolean v32
	){
		int type = 0;
		type = (type<<4) | (v11  ? (v12 ? 2:1):0);
		type = (type<<4) | (v21  ? (v22 ? 2:1):0);
		type = (type<<4) | (v31  ? (v32 ? 2:1):0);
		return type;
	}

	/**
	 * Returns m of the equation: iso = lower*(1-m) + upper*m <br>
	 * which is: m = (iso-lower)/(upper-lower)
	 * @param lower value
	 * @param upper value
	 * @param iso value in between lower and upper
	 * @return m
	 */
	static double interpolateToValue(double lower, double upper, double iso){
		return (iso-lower)/(upper-lower);
	}


}