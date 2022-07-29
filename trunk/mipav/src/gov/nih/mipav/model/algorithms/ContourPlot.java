package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.view.*;

import java.util.*;

import java.awt.*;
import java.awt.event.ActionListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.image.BufferedImage;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.awt.geom.AffineTransform;
import java.util.Arrays;
import java.util.List;
import java.util.function.DoubleBinaryOperator;
import java.util.function.DoubleSupplier;
import java.util.function.IntSupplier;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.edit.PDPageContentStream;



/**
 * The Contours class provides methods to compute contour lines and contour bands
 * from a 2D regular grid of scalar values.
 * See {@link #computeContourLines(double[][], double, int)}
 * and {@link #computeContourBands(double[][], double, double, int, int)}
 * 
 * @author hageldave
 */

public class ContourPlot extends AlgorithmBase {
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
	Code from Contours.java added.  Necessary supporting code from Pixel.java, ColorOperations.interpolateColor,
	 Utils.Copy, Lines.SegmentDetails, and TriangleDetails added.  Need to add code from ContourPlot.java and IsolinesViz.java.
	 Adding code from ContourPlot.	
	 In class Points in public Points link to DefaultGlyph not implemented.
	 In class CoordSysRenderer in renderFallback and renderPDF link to AdaptableView not yet implemented.
	 */
	
	private static ContourPlot cp = new ContourPlot();
	
	public ContourPlot() {
		 
	}
	
	public void runAlgorithm() {
		// formulate bivariate function that defines the 2D surface
		DoubleBinaryOperator bivariateFn = (x,y)->(x*y + x*x - y*y -.01);
		// sample the function
		int hsamples=100, vsamples=100;
		double[][] X = new double[vsamples][hsamples];
		double[][] Y = new double[vsamples][hsamples];
		double[][] Z = new double[vsamples][hsamples];
		for(int i=0; i<vsamples; i++){
			for(int j=0; j<hsamples; j++){
				double x=j*1.0/(hsamples-1), y=i*1.0/(vsamples-1);
				x=x*2-1; y=y*2-1; /* range[-1,1] */
				X[i][j]=x; 
				Y[i][j]=y;
				Z[i][j]=bivariateFn.applyAsDouble(x,y);
			}
		}
		// define colors for isoline levels (3 levels)
		ColorMap colormap = DefaultColorMap.D_COOL_WARM;
		int[] colors = colormap.resample(3, 0, 1).getColors();
		// calculate contour lines from samples for iso values -0.5, 0.0 and 0.5
		List<SegmentDetails> contourA = computeContourLines(
				X,Y,Z, -0.5, colors[0]);
		List<SegmentDetails> contourB = computeContourLines(
				X,Y,Z,  0.0, colors[1]);
		List<SegmentDetails> contourC = computeContourLines(
				X,Y,Z,  0.5, colors[2]);
		// put the contour segments into a Lines object
		Lines contourlines = new Lines();
		contourlines.getSegments().addAll(contourA);
		contourlines.getSegments().addAll(contourB);
		contourlines.getSegments().addAll(contourC);
		// calculate contour bands in between lines
		List<TriangleDetails> bandAB = computeContourBands(
				X,Y,Z, -0.5,0.0, colors[0],colors[1]);
		List<TriangleDetails> bandBC = computeContourBands(
				X,Y,Z,  0.0,0.5, colors[1],colors[2]);
        // put the band triangles into a Triangles object
     	Triangles contourbands = new Triangles();
     	contourbands.setGlobalAlphaMultiplier(0.3);
     	contourbands.getTriangleDetails().addAll(bandAB);
		contourbands.getTriangleDetails().addAll(bandBC);
		// use a coordinate system for display
		CoordSysRenderer coordsys = new CoordSysRenderer();

     	
     	System.out.println("Finished runAlgorithm()");
	} // public void runAlgorithm()
	
	/**
	 * The {@link DefaultColorMap} enum provides predefined {@link ColorMap}s
	 * for different usecases.
	 * <br>
	 * Each {@link DefaultColorMap} has a name starting with either of the
	 * followig letters which indicate the type of color map
	 * <ul>
	 * <li> S - sequential color map for mapping de-/increasing values, e.g.
	 * {@link #S_PLASMA}.
	 * <li> D - diverging color map for mapping values around a pivot value, e.g.
	 * {@link #D_COOL_WARM}.
	 * <li> Q - qualitative color map for mapping categorical values. Qualitative
	 * color map names also specify for how many categories they are suited, e.g.
	 * {@link #Q_12_PAIRED} has discrete colors for up to 12 categories.
	 * </ul><br>
	 * For a visual overview see 
	 * <a href="https://github.com/hageldave/JPlotter/wiki/Color-Maps">
	 * github.com/hageldave/JPlotter/wiki/Color-Maps
	 * </a>
	 * 
	 * @author hageldave
	 */
	
	/**
	 * The ColorMap interface defines discrete mapping from a fixed integer 
	 * interval [0..N-1] to colors through the {@link #getColor(int)} method.
	 * It also defines a continuous mapping from the unit interval [0,1] to 
	 * colors through the {@link #interpolate(double)} method.
	 * For this each of the discrete colors is mapped to a location within
	 * the unit interval [0,1] with the first color mapping to 0 and the last
	 * mapping to 1.
	 * The colors in between have arbitrary but ascending locations within ]0,1[
	 * which can be accessed using {@link #getLocation(int)}.
	 * 
	 * @author hageldave
	 */
	public interface ColorMap {
		
		/**
		 * @return the number of discrete colors in this {@link ColorMap}
		 */
		public int numColors();
		
		/**
		 * Returns the ith color
		 * @param i index of the color
		 * @return ith color in integer packed ARGB format (0xff00ff00 is opaque green)
		 */
		public int getColor(int i);
		
		/**
		 * @return all discrete colors in this {@link ColorMap} 
		 * in integer packed ARGB format (0xff00ff00 is opaque green)
		 */
		public int[] getColors();
		
		/**
		 * Returns the location of the ith color within the unit interval.
		 * @param i index of the location/color
		 * @return location within [0,1]
		 */
		public double getLocation(int i);
		
		/**
		 * @return all locations of the discrete colors in this {@link ColorMap}
		 */
		public double[] getLocations();
		
		
		/**
		 * linearly interpolates colors for the specified location of the unit interval.
		 * @param m location within [0,1]
		 * @return the interpolated color for specified location
		 * in integer packed ARGB format (0xff00ff00 is opaque green)
		 */
		public default int interpolate(double m){
			m = Utils.clamp(0, m, 1);
			int idx = Arrays.binarySearch(getLocations(), (float)m);
			if(idx >= 0){
				return getColor(idx);
			} else {
				idx = -idx -1;
				int c0 = getColor(idx-1);
				int c1 = getColor(idx);
				double m0 = getLocation(idx-1);
				double m1 = getLocation(idx);
				return ColorOperations.interpolateColor(c0, c1, (m-m0)/(m1-m0));
			}
		}

		
		/**
		 * @return copy of this color map
		 */
		public default SimpleColorMap copy(){
			int[] colors = getColors().clone();
			double[] locations = getLocations().clone();
			return cp.new SimpleColorMap(colors, locations);
		}
		
		/**
		 * Returns a uniformly sampled version of this map with the specified
		 * number of samples within the specified interval.
		 * @param numSamples number of uniform samples
		 * @param start of sampling interval within [0,1[
		 * @param end of sampling interval within ]0,1]
		 * @return re-sampled map
		 */
		public default SimpleColorMap resample(int numSamples, double start, double end){
			start = Utils.clamp(0, start, 1);
			end = Utils.clamp(0, end, 1);
			double range = end-start;
			int[] colors = new int[numSamples];
			for(int i = 0; i < numSamples; i++){
				double m = (i*range)/(numSamples-1) + start;
				colors[i] = interpolate(m);
			}
			return cp.new SimpleColorMap(colors);
		}


	} // public interface ColorMap
	
	/**
	 * The ColorOperations class contains methods for manipulating
	 * 32bit ARGB color values.
	 * 
	 * @author hageldave
	 */
	public static class ColorOperations {
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

	}

	public enum DefaultColorMap implements ColorMap {
		D_COOL_WARM(
				0xff_3b4cc0,
				0xff_6688ee,
				0xff_88bbff,
				0xff_b8d0f9,
				0xff_dddddd,
				0xff_f5c4ad,
				0xff_ff9977,
				0xff_dd6644,
				0xff_b40426
				),
		;
		final ColorMap map;
		
		private DefaultColorMap(int[] colors, double[] locations) {
			this.map = cp.new SimpleColorMap(colors, locations);
		}
		
		private DefaultColorMap(int... colors) {
			this.map = cp.new SimpleColorMap(colors);
		}
		
		private DefaultColorMap(ColorMap map){
			this.map = map;
		}

		@Override
		public int numColors() {
			return map.numColors();
		}

		@Override
		public int getColor(int index) {
			return map.getColor(index);
		}
		
		@Override
		public int[] getColors() {
			return map.getColors();
		}

		@Override
		public double getLocation(int index) {
			return map.getLocation(index);
		}

		@Override
		public double[] getLocations() {
			return map.getLocations();
		}

	}

	/**
	 * Implementation of the {@link ColorMap} interface.
	 * 
	 * @author hageldave
	 */
	public class SimpleColorMap implements ColorMap {
		
		protected int[] colors;
		protected double[] locations;
		
		/**
		 * Creates a {@link SimpleColorMap} with the specified colors
		 * and corresponding locations in the unit interval.
		 * As required by the {@link ColorMap} interface the first location has
		 * to be 0, the last location has to 1 and the locations in between have
		 * to monotonically increase within ]0,1[.
		 * 
		 * @param colors integer packed ARGB values (e.g. 0xff00ff00 is opaque green)
		 * @param locations corresponding to colors. Within [0,1].
		 * 
		 * @throws IllegalArgumentException if colors and locations are of different
		 * lengths, if less than 2 colors are specified, if locations does not start
		 * with 0 or does not end with 1, if locations is not sorted in ascending order.
		 */
		public SimpleColorMap(int[] colors, double[] locations) {
			if(Objects.requireNonNull(colors).length != Objects.requireNonNull(locations).length)
				throw new IllegalArgumentException("provided arrays of different lengths");
			if(colors.length < 2)
				throw new IllegalArgumentException("color map needs at least 2 colors");
			if(locations[0] != 0 || locations[locations.length-1] != 1)
				throw new IllegalArgumentException("locations array needs to start with 0 and end with 1");
			if(!Utils.isSorted(Arrays.stream(locations).iterator()))
				throw new IllegalArgumentException("locations have to be sorted in ascending order");
			
			this.colors = colors;
			this.locations = locations;
		}
		
		/**
		 * Creates a new {@link SimpleColorMap} with the specified colors
		 * and uniform spacing.
		 * 
		 * @param colors integer packed ARGB values (e.g. 0xff00ff00 is opaque green)
		 * 
		 * @throws IllegalArgumentException when less than 2 colors are specified.
		 */
		public SimpleColorMap(int... colors) {
			if(Objects.requireNonNull(colors).length < 2)
				throw new IllegalArgumentException("color map needs at least 2 colors");
			this.colors = colors;
			this.locations = IntStream.range(0, colors.length)
					.mapToDouble( i -> i*1.0/(colors.length-1) )
					.toArray();
		}
		
		@Override
		public int numColors(){
			return colors.length;
		}
		
		@Override
		public int getColor(int index){
			return colors[index];
		}
		
		@Override
		public int[] getColors(){
			return colors;
		}
		
		@Override
		public double getLocation(int index){
			return locations[index];
		}
		
		@Override
		public double[] getLocations() {
			return locations;
		}
	} // public class SimpleColorMap
	
	/**
	 * The ColorScheme is responsible for storing and providing color information,
	 * which other components can use.
	 * This enables an easy way to distribute color information to multiple components.
	 * There is the option to use one of the predefined {@link DefaultColorScheme} or to define custom colors.
	 * The color scheme contains five different color attributes,
	 * which can be accessed by the respective components.
	 *
	 * @author lucareichmann
	 */
	public class ColorScheme {
		protected final int color1;
		protected final int color2;
		protected final int color3;
		protected final int color4;
		protected final int colorText;
		protected final int colorBackground;

		/**
		 * Constructor for defining a new color scheme.
		 *
		 * @param primaryColor primary color of the color scheme
		 * @param secondaryColor secondary color of the color scheme
		 * @param tertiaryColor tertiary color of the color scheme
		 * @param quaternaryColor quarternary color of the color scheme
		 * @param textColor text color of the color scheme
		 * @param backgroundColor the background color
		 */
		public ColorScheme (Color primaryColor, Color secondaryColor, Color tertiaryColor, Color quaternaryColor, Color textColor, Color backgroundColor) {
			this(primaryColor.getRGB(),secondaryColor.getRGB(),tertiaryColor.getRGB(),quaternaryColor.getRGB(),textColor.getRGB(), backgroundColor.getRGB());
		}
		
		/**
		 * Constructor for defining a new color scheme.
		 *
		 * @param primaryColor primary color of the color scheme (integer packed ARGB)
		 * @param secondaryColor secondary color of the color scheme (integer packed ARGB)
		 * @param tertiaryColor tertiary color of the color scheme (integer packed ARGB)
		 * @param quaternaryColor quarternary color of the color scheme (integer packed ARGB)
		 * @param textColor text color of the color scheme (integer packed ARGB)
		 * @param backgroundColor the background color (integer packed ARGB)
		 */
		public ColorScheme (int primaryColor, int secondaryColor, int tertiaryColor, int quaternaryColor, int textColor, int backgroundColor) {
			this.color1 = primaryColor;
			this.color2 = secondaryColor;
			this.color3 = tertiaryColor;
			this.color4 = quaternaryColor;
			this.colorText = textColor;
			this.colorBackground = backgroundColor;
		}

		/**
		 * @return primary color of the color scheme (integer packed ARGB)
		 */
		public int getColor1() {
			return this.color1;
		}

		/**
		 * @return secondary color of the color scheme (integer packed ARGB)
		 */
		public int getColor2() {
			return this.color2;
		}

		/**
		 * @return tertiary color of the color scheme (integer packed ARGB)
		 */
		public int getColor3() {
			return this.color3;
		}

		/**
		 * @return fourth color of the color scheme (integer packed ARGB)
		 */
		public int getColor4 () {
			return this.color4;
		}

		/**
		 * @return text color of the color scheme (integer packed ARGB)
		 */
		public int getColorText() {
			return this.colorText;
		}
		
		/**
		 * @return background color of the scheme (integer packed ARGB)
		 */
		public int getColorBackground() {
			return colorBackground;
		}
	} // public class ColorScheme
	
	/**
	 * Enum containing predefined {@link ColorScheme}s,
	 * which can be accessed through {@link #get()}.
	 *
	 * @author lucareichmann
	 */
	public enum DefaultColorScheme {
		LIGHT(
			cp.new ColorScheme(
				Color.BLACK,
				Color.GRAY,
				Color.DARK_GRAY,
				new Color(0xdddddd),
				new Color(96, 96, 96),
				Color.WHITE
			)
		),
		DARK(
			cp.new ColorScheme(
				0xffdddddd,
				0xffaaaaaa,
				0xff666666,
				0xff444444,
				0xffbbbbbb,
				0xff21232b
			)
		),
		;

		private final ColorScheme scheme;

		private DefaultColorScheme (ColorScheme scheme) {
			this.scheme = scheme;
		}

		/**
		 * Returns the preset's {@link ColorScheme} object
		 * @return color scheme
		 */
		public ColorScheme get() {
			return this.scheme;
		}
	} // public enum DefaultColorScheme

	
	/**
	 * Class containing utility methods
	 * 
	 * @author hageldave
	 */
	public static class Utils {

		
			
			/**
			 * Copies the specified {@link Point2D} (calls clone) and
			 * casts the copy to the class of the original.
			 * @param p point to copy
			 * @return the copied point
			 * 
			 * @param <T> type of Point2D
			 */
			@SuppressWarnings("unchecked")
			public static <T extends Point2D> T copy(T p){
				return (T) p.clone();
			}
			
			/**
			 * Copies the specified {@link RectangularShape} (calls clone) and
			 * casts the copy to the class of the original.
			 * @param r rectangle to copy
			 * @return the copied rectangle
			 * 
			 * @param <T> type of RectangularShape
			 */
			@SuppressWarnings("unchecked")
			public static <T extends RectangularShape> T copy(T r){
				return (T) r.clone();
			}

			
			/**
			 * Checks if specified iterator is sorted according to natural ordering.
			 * @param iter iterator
			 * @return true if sorted, false otherwise
			 * @param <T> element type that implements {@link Comparable}
			 */
			public static <T extends Comparable<T>> boolean isSorted(Iterator<T> iter){
				T prev = iter.next();
				while(iter.hasNext()){
					T next = iter.next();
					if(next.compareTo(prev) < 0){
						return false;
					}
				}
				return true;
			}
			
			/**
			 * Syntactic sugar for conditional stream.parallel().
			 * @param stream to make parallel
			 * @param parallel whether to make parallel or not
			 * @return stream.parallel() if true
			 * 
			 * @param <T> element type of stream
			 */
			public static <T> Stream<T> parallelize(Stream<T> stream, boolean parallel){
				return parallel ? stream.parallel():stream;
			}

			
			/**
			 * Clamps value between specified bounds
			 * @param lower minimum value
			 * @param v value to clamp
			 * @param upper maximum value
			 * @return max(lower,min(upper,v))
			 */
			public static double clamp(double lower, double v, double upper){
				return Math.max(lower, Math.min(upper, v));
			}
			
			/**
			 * Clamps value between specified bounds
			 * @param lower minimum value
			 * @param v value to clamp
			 * @param upper maximum value
			 * @return max(lower,min(upper,v))
			 */
			public static float clamp(float lower, float v, float upper){
				return Math.max(lower, Math.min(upper, v));
			}
			
			/**
			 * Clamps value between specified bounds
			 * @param lower minimum value
			 * @param v value to clamp
			 * @param upper maximum value
			 * @return max(lower,min(upper,v))
			 */
			public static int clamp(int lower, int v, int upper){
				return Math.max(lower, Math.min(upper, v));
			}
			
			/**
			 * Returns the minimum of 3 values
			 * @param v0 value
			 * @param v1 value
			 * @param v2 value
			 * @return minimum
			 */
			public static double min3(double v0, double v1, double v2){
				return Math.min(Math.min(v0, v1), v2);
			}

			/**
			 * tests intersection or containment of rectangle and triangle.
			 * @param rect rectangle to test
			 * @param x0 x coordinate of 0th triangle vertex
			 * @param y0 y coordinate of 0th triangle vertex
			 * @param x1 x coordinate of 1st triangle vertex
			 * @param y1 y coordinate of 1st triangle vertex
			 * @param x2 x coordinate of 2nd triangle vertex
			 * @param y2 y coordinate of 2nd triangle vertex
			 * @return true when intersecting
			 */
			public static boolean rectIntersectsOrIsContainedInTri(
					Rectangle2D rect, 
					double x0, double y0, 
					double x1, double y1, 
					double x2, double y2)
			{
				
				return 
						rect.intersectsLine(x0, y0, x1, y1) || 
						rect.intersectsLine(x0, y0, x2, y2) ||
						rect.intersectsLine(x2, y2, x1, y1) ||
						pointInTri(rect.getX(), rect.getY(), x0, y0, x1, y1, x2, y2);
			}
			
			private static double sign (
					double x1, double y1, 
					double x2, double y2, 
					double x3, double y3)
			{
			    return (x1 - x3) * (y2 - y3) - (x2 - x3) * (y1 - y3);
			}
			
			private static boolean pointInTri(
					double px, double py, 
					double x0, double y0, 
					double x1, double y1, 
					double x2, double y2)
			{
				double d1, d2, d3;
			    boolean has_neg, has_pos;

			    d1 = sign(px,py, x0,y0, x1,y1);
			    d2 = sign(px,py, x1,y1, x2,y2);
			    d3 = sign(px,py, x2,y2, x0,y0);

			    has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
			    has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);

			    return !(has_neg && has_pos);
			}



		}
		
	/**
	 * Interface for an object that can be rendered by a {@link Renderer} e.g. the {@link GenericRenderer}.
	 * It is intended for objects that contain openGL resources such as a {@link VertexArray} for example.
	 * For this reason the interface extends the {@link AutoCloseable} interface for disposing of GL resources
	 * on {@link #close()}.
	 * Its state can be dirty ({@link #isDirty()}) which means that the GL resources are not in sync with the object and implies that
	 * a call to the {@link #updateGL(boolean)} method is necessary before rendering.
	 * The {@link #initGL()} method has to be called once before the first rendering to allocate required GL resources.
	 * 
	 * @author hageldave
	 */
	//public interface Renderable extends AutoCloseable {
	public interface Renderable {
		/**
		 * tests if this {@link Renderable} intersects or contains the specified
		 * rectangle.
		 * @param rect rectangle to test
		 * @return true when intersecting
		 */
		public boolean intersects(Rectangle2D rect);
		
		/**
		 * Indicates whether this Renderable is hidden i.e. will not be drawn.
		 * @return true when hidden
		 */
		public default boolean isHidden() {return false;}
		
		/**
		 * if true, indicates that a call to {@link #updateGL(boolean)} is necessary to sync 
		 * this objects GL resources to its current state.
		 * @return true if dirty
		 */
		public boolean isDirty();


	}
	
	/**
	 * The {@link CompleteRenderer} comprises a {@link LinesRenderer},
	 * a {@link PointsRenderer}, a {@link TextRenderer} and 
	 * a {@link TrianglesRenderer}.
	 * It thus can render the most important graphical elements for
	 * a scientific 2D visualization (hence its name).
	 * <p>
	 * The order in which these Renderers are processed by default is:
	 * <ol>
	 * <li>{@link Triangles}</li>
	 * <li>{@link Lines}</li>
	 * <li>{@link Curves}</li>
	 * <li>{@link Points}</li>
	 * <li>{@link Text}</li>
	 * </ol>
	 * This implies that Lines will be drawn over Triangles,
	 * Curves over Lines, Points over Curves, and Text over Points.
	 * Using the {@link #setRenderOrder(int, int, int, int, int)} method
	 * the order of renderers can be changed.
	 * <p>
	 * To add {@link Renderable}s to this Renderer either use the public attributes
	 * {@link #triangles}, {@link #lines}, {@link #curves} , {@link #points}, {@link #text} 
	 * to directly access the desired renderer or use the {@link #addItemToRender(Renderable)}
	 * method.
	 * 
	 * @author hageldave
	 */
	public class CompleteRenderer implements Renderer {
		
		public final LinesRenderer lines = new LinesRenderer();
		//public final PointsRenderer points = new PointsRenderer();
		public final TextRenderer text = new TextRenderer();
		//public final TrianglesRenderer triangles = new TrianglesRenderer();
		//public final CurvesRenderer curves = new CurvesRenderer();
		
		private final Renderer[] rendererLUT = new Renderer[]{/*triangles,*/lines,/*curves,points,*/text};
		public static final int TRI = 0, LIN = 1, PNT = 2, TXT = 3, CRV = 4;
		private final int[] renderOrder = {/*TRI,*/LIN,/*CRV,PNT,*/TXT};
		boolean isEnabled = true;
		
		public boolean isEnabled() {
			return isEnabled;
		}
		
		public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
			if(!isEnabled()){
				return;
			}

			rendererLUT[renderOrder[0]].renderFallback(g, p, w, h);
			rendererLUT[renderOrder[1]].renderFallback(g, p, w, h);
			//rendererLUT[renderOrder[2]].renderFallback(g, p, w, h);
			//rendererLUT[renderOrder[3]].renderFallback(g, p, w, h);
			//rendererLUT[renderOrder[4]].renderFallback(g, p, w, h);
		}
		
		public void renderPDF(PDDocument doc, PDPage page, int x, int y, int w, int h) {
			if(!isEnabled()){
				return;
			}
			rendererLUT[renderOrder[0]].renderPDF(doc, page, x, y, w, h);
			rendererLUT[renderOrder[1]].renderPDF(doc, page, x, y, w, h);
			//rendererLUT[renderOrder[2]].renderPDF(doc, page, x, y, w, h);
			//rendererLUT[renderOrder[3]].renderPDF(doc, page, x, y, w, h);
			//rendererLUT[renderOrder[4]].renderPDF(doc, page, x, y, w, h);
		}
	}
	
	/**
	 * The Glyph interface has to be implemented by a class that realizes
	 * a graphical representation of a 2D point (e.g. a cross or a circle).
	 * The interface declares the {@link #fillVertexArray(VertexArray)} method 
	 * that fills a vertex array so that it contains the vertices for the 
	 * shape of the glyph.
	 * The other methods describe how the glyph has to be drawn
	 * by the {@link PointsRenderer}.
	 * 
	 * @author hageldave
	 */
	public interface Glyph {

		/**
		 * Fills the the first attribute (idx=0) of the specified vertex array
		 * with the vertices that make up the shape of this glyph.
		 * If necessary the ELEMENT_ARRAY_BUFFER (indices) of the VA can also be
		 * written to.
		 * The glyphs vertices should be centered around the origin (0,0), 
		 * it is also recommended that the vertices are within unit object coordinates
		 * i.e. {@code (x,y) in [-0.5, 0.5]}.
		 * 
		 * @param va the VertexArray to fill
		 */
		//public void fillVertexArray(VertexArray va);
		
		/**
		 * @return number of vertices this glyph has.
		 */
		public int numVertices();
		
		/**
		 * @return the GL primitive to be used for drawing.
		 * For example {@link GL11#GL_LINES} or {@link GL11#GL_TRIANGLE_STRIP}
		 */
		public int primitiveType();

		/**
		 * @return the size in pixels this glyph should be scaled to.
		 */
		public int pixelSize();
		
		/**
		 * @return true if an elements draw call is to be used e.g. 
		 * {@link GL31#glDrawElementsInstanced(int, int, int, long, int)},
		 * false if an arrays draw call is to be used e.g.
		 * {@link GL31#glDrawArraysInstanced(int, int, int, int)}.<br>
		 * Of course an elements draw call can only be used if the VertexArray 
		 * has indices, i.e. the ELEMENT_ARRAY_BUFFER is set.
		 * A {@link Glyph} implementation has to guarantee for that.
		 */
		public boolean useElementsDrawCall();

		/**
		 * Creates SVG elements that represent this Glyph in an
		 * SVG context. This is used to create an SVG {@code symbol}.
		 * The {@link #isFilled()} method determines whether the "fill"
		 * or "stroke" attribute is used for coloring an instance of the symbol.
		 * @param doc to create the elements with.
		 * @return list of SVG elements.
		 */
		//public List<Element> createSVGElements(Document doc);

		/**
		 * Creates PDF elements that will be appended to the content stream.
		 * This is used to create an SVG {@code symbol}.
		 * The {@link #isFilled()} method determines whether the "fill"
		 * or "stroke" attribute is used for coloring an instance of the symbol.
		 * @param contentStream content stream that the glyph will be added to
		 * @return content stream that holds the glyph written to the document
		 */
		public PDPageContentStream createPDFElement(PDPageContentStream contentStream);

		/**
		 * Fallback rendering path when OpenGL is not available.
		 * Draws this glyph to the specified {@link Graphics2D} object
		 * with the specified scaling.
		 * @param g graphics to draw to
		 * @param scaling factor by which to scale this glyph
		 */
		public void drawFallback(Graphics2D g, float scaling);
		
		/**
		 * @return name of this Glyph, used as part of an SVG identifier for
		 * the corresponding symbol definition.
		 */
		public String glyphName();
		
		/**
		 * @return whether this glyph is filled or not, e.g. a cross is not filled but a circle can.
		 */
		public boolean isFilled();
	}
	
	/**
	 * Class for storing all the details of a single point to be rendered.
	 * This comprises location, color, scaling, glyph rotation and picking color.
	 * @author hageldave
	 */
	public static class PointDetails implements Cloneable {
		public Point2D location;
		public DoubleSupplier rot;
		public DoubleSupplier scale;
		public IntSupplier color;
		public int pickColor;
		
		public PointDetails(Point2D location) {
			this.location = location;
			this.rot = ()->0;
			this.scale = ()->1;
			this.color = ()->0xff555555;
		}
		
		/**
		 * Returns a shallow copy of this point with deep copied
		 * {@link #location}.
		 * @return copy of this point
		 */
		public PointDetails copy() {
			try {
				PointDetails clone = (PointDetails) super.clone();
				clone.location = Utils.copy(clone.location);
				return clone;
			} catch (CloneNotSupportedException e) {
				// this shouldn't happen, since we are Cloneable
				throw new InternalError(e);
			}
		}
		
		public PointDetails clone() {
			try {
				PointDetails clone = (PointDetails) super.clone();
				return clone;
			} catch (CloneNotSupportedException e) {
				// this shouldn't happen, since we are Cloneable
				throw new InternalError(e);
			}
		}
		
		/**
		 * Sets the rotation of the glyph for this point
		 * @param rot rotation in radian
		 * @return this for chaining
		 */
		public PointDetails setRotation(double rot){
			return setRotation(()->rot);
		}
		
		/**
		 * Sets the rotation of the glyph for this point
		 * @param rotation in radian
		 * @return this for chaining
		 */
		public PointDetails setRotation(DoubleSupplier rotation){
			this.rot = rotation;
			return this;
		}
		
		/**
		 * Sets the scaling of this point's glyph		
		 * @param scale scaling
		 * @return this for chaining
		 */
		public PointDetails setScaling(DoubleSupplier scale){
			this.scale = scale;
			return this;
		}
		
		/**
		 * Sets the scaling of this point's glyph		
		 * @param scale scaling
		 * @return this for chaining
		 */
		public PointDetails setScaling(double scale){
			return setScaling(()->scale);
		}
		
		/**
		 * Sets this point's color
		 * @param color integer packed ARGB color value of the glyph for the point (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public PointDetails setColor(IntSupplier color){
			this.color = color;
			return this;
		}
		
		/**
		 * Sets this point's color
		 * @param color integer packed ARGB color value of the glyph for the point (e.g. 0xff00ff00 = opaque green)
		 * @return this for chaining
		 */
		public PointDetails setColor(int color){
			return setColor(()->color);
		}
		
		/**
		 * Sets this point's color
		 * @param color of this point's glyph
		 * @return this for chaining
		 */
		public PointDetails setColor(Color color){
			return setColor(color.getRGB());
		}
		
		/**
		 * Sets the picking color.
		 * When a non 0 transparent color is specified its alpha channel will be set to 0xff to make it opaque.
		 * @param pickID picking color of the point (see {@link Points} for details)
		 * @return this for chaining
		 */
		public PointDetails setPickColor(int pickID){
			if(pickID != 0)
				pickID = pickID | 0xff000000;
			this.pickColor = pickID;
			return this;
		}
	}
	
	/**
	 * The default implementations of various {@link Glyph}s such as a
	 * {@link #CROSS}, {@link #CIRCLE}, {@link #SQUARE} and {@link #TRIANGLE} glyph.
	 * Apart from these there are {@link #ARROW} and {@link #ARROWHEAD} glyphs that can be used for
	 * directional information as in vector field visualizations.
	 * 
	 * @author hageldave
	 */
	/*public enum DefaultGlyph implements Glyph {
		// a filled circle glyph (20 line segments)
		CIRCLE_F(DefaultGlyph::mkCircleWithCenter, 22, GL11.GL_TRIANGLE_FAN, 8, true, true, 
				DefaultGlyph::mkCircleSVG, DefaultGlyph::mkCirclePDF, DefaultGlyph::drawCircleF),
		;
		
		private Consumer<VertexArray> vertexGenerator;
		private int numVertices;
		private int primitiveType;
		private int pixelSize;
		private boolean drawAsElements;
		private boolean isFilled;
		private BiFunction<Document,Integer,List<Element>> svgElementGenerator;
		private BiFunction<PDPageContentStream,Integer,PDPageContentStream> pdfElementGenerator;
		private Graphics2DDrawing fallbackDraw;
		
		private DefaultGlyph(Consumer<VertexArray> vertGen, int numVerts, int primType, int pixelSize, boolean elements, boolean isFilled, BiFunction<Document,Integer,List<Element>> svgGen, BiFunction<PDPageContentStream,Integer,PDPageContentStream> pdfGen, Graphics2DDrawing fallbackDraw) {
			this.vertexGenerator = vertGen;
			this.numVertices = numVerts;
			this.primitiveType = primType;
			this.pixelSize = pixelSize;
			this.drawAsElements = elements;
			this.isFilled = isFilled;
			this.svgElementGenerator = svgGen;
			this.pdfElementGenerator = pdfGen;
			this.fallbackDraw = fallbackDraw;
		}
		
		static void drawCircleF(Graphics2D g, int pixelSize, float scaling) {
			float[][] verts = new float[2][numCircVerts];
			for(int i=0; i<numCircVerts;i++){
				verts[0][i] = sincosLUT[1][i]*0.5f*pixelSize*scaling;
				verts[1][i] = sincosLUT[0][i]*0.5f*pixelSize*scaling;
			}
			g.fill(new Polygon2D(verts[0], verts[1], numCircVerts));
		}
		
		@Override
		public void drawFallback(Graphics2D g, float scaling) {
			this.fallbackDraw.draw(g, pixelSize, scaling);
		}



	}*/

	
	/**
	 * The Points class is a collection of 2D points that are to be represented
	 * using the same {@link Glyph} (the graphical representation of a point).
	 * A point instance in this collection has the following attributes.
	 * <ul>
	 * <li>position - the 2D location of the point</li>
	 * <li>scaling - the scaling of the glyph it is represented with</li>
	 * <li>rotation - the rotation of the glyph it is represented with</li>
	 * <li>color - the color with wich the glyph it is renered</li>
	 * <li>picking color - the picking color with which the glyph is rendered into the (invisible) picking color attachment
	 * of an {@link FBO}. This color may serve as an identifier of the object that can be queried from a location of the
	 * rendering canvas. It may take on a value in range of 0xff000001 to 0xffffffff (16.777.214 possible values) or 0.
	 * </li>
	 * </ul>
	 * Apart from these per point attributes, this {@link Points} class features a global scaling parameter by which all
	 * point instances of this collection will be scaled at rendering ({@link #setGlobalScaling(double)}).
	 * Also a global alpha multiplier which scales every points color alpha value, which can be used to introduce transparency
	 * for all points of this collection, which may come in handy to visualize density when plotting a huge amount of points.
	 * 
	 * @author hageldave
	 */
	public class Points implements Renderable {

		public Glyph glyph;
		//protected VertexArray va;
		protected boolean isDirty;
		protected DoubleSupplier globalScaling = ()->1.0;
		protected DoubleSupplier globalAlphaMultiplier = ()->1.0;
		protected DoubleSupplier globalSaturationMultiplier = () -> 1.0;
		protected ArrayList<PointDetails> points = new ArrayList<>();
		protected boolean hidden=false;
		protected boolean useVertexRounding=false;
		protected boolean isGLDoublePrecision = false;
		
		/**
		 * Creates a new {@link Points} object which uses {@link DefaultGlyph#CIRCLE_F} for displaying its points.
		 */
		public Points() {
			//this(DefaultGlyph.CIRCLE_F);
		}
		
		/**
		 * @return the number of points in this in this {@link Points} object.
		 */
		public int numPoints(){
			return points.size();
		}

		
		@Override
		public boolean isDirty() {
			return isDirty;
		}
		
		@Override
		public boolean intersects(Rectangle2D rect) {
			boolean useParallelStreaming = numPoints() > 10000;
			return Utils.parallelize(getPointDetails().stream(), useParallelStreaming)
					.filter(p->rect.contains(p.location))
					.findAny()
					.isPresent();
		}
		
		/**
		 * @return the list of point details.<br>
		 * Make sure to call {@link #setDirty()} when manipulating.
		 */
		public ArrayList<PointDetails> getPointDetails() {
			return points;
		}


	}
	
	/**
	 * The Legend class is {@link Renderable} and its own {@link Renderer} at once.
	 * It is intended to be used to display labels and corresponding visual representatives 
	 * such as a colored {@link Glyph} or line segment, in order to explain the meaning 
	 * of the contents of a visualization.
	 * <p>
	 * To add items to the legend, the methods {@link #addGlyphLabel(Glyph, int, String)} and 
	 * {@link #addLineLabel(double, int, String)} can be used.
	 * The layout of the items is very similar to {@link FlowLayout} in which the items
	 * are positioned next to each other until no more space is available to the right
	 * and a line break happens, then positioning continues in the next row.
	 * A slight difference is that glyph labels are always first in order and followed by
	 * line labels.
	 * Layouting happens on {@link #updateGL(boolean)}.
	 * 
	 * @author hageldave
	 */
	public static class Legend implements Renderable, Renderer {

		protected ArrayList<GlyphLabel> glyphLabels = new ArrayList<>(0);

		protected ArrayList<LineLabel> lineLabels = new ArrayList<>(0);

		protected ArrayList<ColormapLabel> colormapLabels = new ArrayList<>(0);

		protected Map<Glyph, Points> glyph2points = new LinkedHashMap<>();

		protected Map<Integer, Lines> pattern2lines = new LinkedHashMap<>();

		protected LinkedList<Triangles> triangles = new LinkedList<>();

		protected LinkedList<Text> texts = new LinkedList<>();

		protected CompleteRenderer delegate = cp.new CompleteRenderer();

		protected boolean isDirty = true;

		protected int viewPortWidth = 0;

		protected int viewPortHeight = 0;

		protected boolean isEnabled=true;

		protected ColorScheme colorScheme;

		public Legend() {
			this.colorScheme = DefaultColorScheme.LIGHT.get();
		}
		
		protected static class GlyphLabel {
			public String labelText;
			public Glyph glyph;
			public int color;
			public int pickColor;

			public GlyphLabel(String labelText, Glyph glyph, int color, int pickColor) {
				this.labelText = labelText;
				this.glyph = glyph;
				this.color = color;
				this.pickColor = pickColor;
			}
		}

		protected static class LineLabel {
			public String labelText;
			public double thickness;
			public int color;
			public int pickColor;
			public int strokePattern;

			public LineLabel(String labelText, double thickness, int color, int pickColor, int strokePattern) {
				this.labelText = labelText;
				this.thickness = thickness;
				this.color = color;
				this.pickColor = pickColor;
				this.strokePattern = strokePattern;
			}


			public LineLabel(String labelText, double thickness, int color, int pickColor){
				this(labelText, thickness, color, pickColor, 0xffff);
			}
		}

		protected static class ColormapLabel {
			public String labelText;
			public ColorMap cmap;
			public boolean vertical;
			public int pickColor;
			public double[] ticks;
			public String[] ticklabels;

			public ColormapLabel(String labelText, ColorMap cmap, boolean vertical, int pickColor, double[] ticks, String[] ticklabels) {
				this.labelText = labelText;
				this.cmap = cmap;
				this.vertical = vertical;
				this.pickColor = pickColor;
				this.ticks = ticks == null ? new double[0]:ticks;
				this.ticklabels = ticklabels == null ? new String[0]:ticklabels;
			}

			public ColormapLabel(String labelText, ColorMap cmap, boolean vertical, int pickColor, double[] ticks) {
				this(labelText, cmap, vertical, pickColor, ticks, null);
			}

			public ColormapLabel(String labelText, ColorMap cmap, boolean vertical, int pickColor) {
				this(labelText, cmap, vertical, pickColor, null, null);
			}
		}

		
		public void setColorScheme(final ColorScheme colorScheme) {
			this.colorScheme = colorScheme;
			setDirty();
		}

		/**
		 * Sets the {@link #isDirty()} state of this legend to true.
		 * This indicates that a call to {@link #updateGL(boolean)} is necessary
		 * to sync GL resources with this legends state.
		 * @return this for chaining
		 */
		public Legend setDirty() {
			this.isDirty = true;
			return this;
		}
		
		@Override
		public boolean isDirty() {
			return isDirty;
		}


		
		@Override
		public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
			if(!isEnabled()){
				return;
			}
			if(w == 0 || h == 0){
				return;
			}
			if(isDirty() || viewPortWidth != w || viewPortHeight != h){
				viewPortWidth = w;
				viewPortHeight = h;
				//updateGL(false); // only clearGL requires GL context, but all GL resources are null, so no prob.
			}
			delegate.renderFallback(g, p, w, h);
		}

		
		@Override
		public void renderPDF(PDDocument doc, PDPage page, int x, int y, int w, int h) {
			if(!isEnabled()){
				return;
			}
			delegate.renderPDF(doc, page, x, y, w, h);
		}


		
		@Override
		public boolean isEnabled() {
			return isEnabled;
		}
		
		/**
		 * Always return false.
		 */
		@Override
		public boolean intersects(Rectangle2D rect) {
			return false;
		}



	}


	
	/**
	 * The Lines class is a collection of linear line segments.
	 * Each segment is defined by a 2D start and end point and can
	 * be colored per point. This means that when the colors at start
	 * and endpoint are different the line will be rendered with a
	 * linear color gradient which is interpolated between the two points.
	 * Per default the thickness of the line segments is 1 pixel but can be
	 * altered for all segments in a {@link Lines} object (not per segment).
	 * Each segment has a single picking color.
	 * The picking color is the color with which the segment is rendered into the (invisible) picking color attachment
	 * of an {@link FBO}. This color may serve as an identifier of the object that can be queried from a location of the
	 * rendering canvas. It may take on a value in range of 0xff000001 to 0xffffffff (16.777.214 possible values) or 0.
	 * <p>
	 * There is also a global alpha multiplier ({@link #setGlobalAlphaMultiplier(double)}) 
	 * which scales every segments color alpha value, which can be used to introduce transparency for all segments of this 
	 * collection. This may come in handy to visualize density when plotting a huge amount of lines. <br>
	 * Similarly, the global thickness multiplier ({@link #setGlobalThicknessMultiplier(double)}) can be used to
	 * scale every segment's thickness of this Lines object by a specific factor.
	 * <p>
	 * The segments of this object can be rendered using a stroke pattern ({@link #setStrokePattern(int)}) to draw
	 * dashed or dotted lines.
	 * 
	 * @author hageldave
	 */
	public class Lines implements Renderable {

		//protected VertexArray va;

		protected ArrayList<SegmentDetails> segments = new ArrayList<>();

		protected DoubleSupplier globalSaturationMultiplier = () -> 1.0;

		protected DoubleSupplier globalThicknessMultiplier = () -> 1.0;

		protected boolean isDirty = true;

		protected DoubleSupplier globalAlphaMultiplier = () -> 1.0;

		protected boolean useVertexRounding=false;

		protected short strokePattern = (short)0xffff;
		
		protected float strokeLength = 16;
		
		protected boolean hidden = false;
		
		/**
		 * Sets the {@link #isDirty()} state of this renderable to true.
		 * This indicates that an {@link #updateGL(boolean, double, double)} call is necessary to sync GL resources.
		 * @return this for chaining
		 */
		public Lines setDirty() {
			this.isDirty = true;
			return this;
		}
		
		@Override
		public boolean isDirty() {
			return isDirty;
		}


		
		/**
		 * @return the number of line segments in this {@link Lines} object
		 */
		public int numSegments() {
			return segments.size();
		}
		
		/**
		 * Adds a new line segment to this object.
		 * Sets the {@link #isDirty()} state to true.
		 * @param p1 start point
		 * @param p2 end point
		 * @return the added segment
		 */
		public SegmentDetails addSegment(Point2D p1, Point2D p2){
			SegmentDetails seg = new SegmentDetails(p1, p2);
			segments.add(seg);
			setDirty();
			return seg;
		}

		/**
		 * Adds a new line segment to this object.
		 * Sets the {@link #isDirty()} state to true.
		 * @param x1 x coordinate of start point
		 * @param y1 y coordinate of start point
		 * @param x2 x coordinate of end point
		 * @param y2 y coordinate of end point
		 * @return the added segment
		 */
		public SegmentDetails addSegment(double x1, double y1, double x2, double y2){
			return addSegment(new Point2D.Double(x1, y1), new Point2D.Double(x2, y2));
		}

		
		/**
		 * @return the line segments list.
		 * Make sure to call {@link #setDirty()} when manipulating.
		 */
		public ArrayList<SegmentDetails> getSegments() {
			return segments;
		}

		/**
		 * Removes all segments of this object.
		 * Sets the {@link #isDirty()} state to true.
		 * @return this for chaining
		 */
		public Lines removeAllSegments() {
			this.segments.clear();
			return setDirty();
		}

		
		@Override
		public boolean intersects(Rectangle2D rect) {
			boolean useParallelStreaming = numSegments() > 1000;
			return Utils.parallelize(getSegments().stream(), useParallelStreaming)
					.filter(seg->rect.intersectsLine(seg.p0.getX(), seg.p0.getY(), seg.p1.getX(), seg.p1.getY()))
					.findAny()
					.isPresent();
		}
		
		@Override
		public boolean isHidden() {
			return hidden;
		}
		
		/**
		 * En/Disables vertex rounding for this Lines object. This indicates if
		 * the {@link LinesRenderer}'s shader will round vertex positions of 
		 * the quad vertices (that a segment is expanded to) to integer values.
		 * <p>
		 * This has the effect of sharpening horizontal and vertical lines, but
		 * can affect differently oriented lines to shrink in thickness or even vanish. 
		 * <p>
		 * Also this only makes sense when the Lines object is of integer valued thickness.
		 * @param useVertexRounding will enable if true
		 * @return this for chaining
		 */
		public Lines setVertexRoundingEnabled(boolean useVertexRounding) {
			this.useVertexRounding = useVertexRounding;
			return this;
		}
		
		/**
		 * Sets the line thickness multiplier for this {@link Lines} object in pixels.
		 * The effective thickness of a segment results from multiplication of its 
		 * thickness with this value.
		 * @param thickness of the lines, default is 1.
		 * @return this for chaining
		 */
		public Lines setGlobalThicknessMultiplier(DoubleSupplier thickness) {
			this.globalThicknessMultiplier = thickness;
			return this;
		}

		/**
		 * Sets the line thickness multiplier for this {@link Lines} object in pixels.
		 * The effective thickness of a segment results from multiplication of its 
		 * thickness with this value.
		 * @param thickness of the lines, default is 1.
		 * @return this for chaining
		 */
		public Lines setGlobalThicknessMultiplier(double thickness) {
			return setGlobalThicknessMultiplier(() -> thickness); 
		}

		/**
		 * @return the line thickness multiplier of this {@link Lines} object
		 */
		public float getGlobalThicknessMultiplier() {
			return (float)globalThicknessMultiplier.getAsDouble();
		}



	}
	
	/**
	 * The Triangles class is a collection of 2D triangles.
	 * A single triangle consists of three 2D points where each of these points
	 * can be colored differently which leads to the triangle area being colored
	 * by interpolating using barycentric coordinates.
	 * Also a triangle can have a single picking color, which is the color with 
	 * which the triangle is rendered into the (invisible) picking color attachment
	 * of an {@link FBO}. 
	 * This color may serve as an identifier of the object that can be queried from 
	 * a location of the rendering canvas. 
	 * It may take on a value in range of 0xff000001 to 0xffffffff (16.777.214 possible values) or 0.
	 * <p>
	 * There is also a global alpha multiplier parameter which scales every triangle's color alpha value,
	 * which can be used to introduce transparency for all triangles of this collection.
	 * This may come in handy to let other rendered content under a triangle 'shine through'.
	 * 
	 * @author hageldave
	 */
	public class Triangles implements Renderable {

		//protected VertexArray va;
		protected boolean isDirty = true;
		protected DoubleSupplier globalAlphaMultiplier = ()->1.0;
		protected DoubleSupplier globalSaturationMultiplier = () -> 1.0;
		protected ArrayList<TriangleDetails> triangles = new ArrayList<>();
		protected boolean useCrispEdgesForSVG = true;
		protected boolean useAAinFallback = false;
		protected boolean hidden=false;
		
		/**
		 * @return the number of triangles in this collection.
		 */
		public int numTriangles() {
			return triangles.size();
		}

		
		@Override
		public boolean intersects(Rectangle2D rect) {
			boolean useParallelStreaming = numTriangles() > 1000;
			return Utils.parallelize(getTriangleDetails().stream(), useParallelStreaming)
					.filter(tri->Utils.rectIntersectsOrIsContainedInTri(
							rect, 
							tri.p0.getX(), tri.p0.getY(), 
							tri.p1.getX(), tri.p1.getY(), 
							tri.p2.getX(), tri.p2.getY()
							))
					.findAny()
					.isPresent();
		}
		
		@Override
		public boolean isHidden() {
			return hidden;
		}
		
		/**
		 * @return the list of triangle details.<br>
		 * Make sure to call {@link #setDirty()} when manipulating.
		 */
		public ArrayList<TriangleDetails> getTriangleDetails() {
			return triangles;
		}
		
		/**
		 * Sets the global alpha multiplier parameter of this {@link Triangles} object.
		 * The value will be multiplied with each vertex' alpha color value when rendering.
		 * The triangle will then be rendered with the opacity {@code alpha = globalAlphaMultiplier * point.alpha}.
		 * @param globalAlphaMultiplier of the triangles in this collection
		 * @return this for chaining
		 */
		public Triangles setGlobalAlphaMultiplier(double globalAlphaMultiplier) {
			return setGlobalAlphaMultiplier(()->globalAlphaMultiplier);
		}
		
		/**
		 * Sets the global alpha multiplier parameter of this {@link Triangles} object.
		 * The value will be multiplied with each vertex' alpha color value when rendering.
		 * The triangle will then be rendered with the opacity {@code alpha = globalAlphaMultiplier * point.alpha}.
		 * @param globalAlphaMultiplier of the triangles in this collection
		 * @return this for chaining
		 */
		public Triangles setGlobalAlphaMultiplier(DoubleSupplier globalAlphaMultiplier) {
			this.globalAlphaMultiplier = globalAlphaMultiplier;
			return this;
		}

		/**
		 * @return the global alpha multiplier of the triangles in this collection
		 */
		public float getGlobalAlphaMultiplier() {
			return (float)globalAlphaMultiplier.getAsDouble();
		}

		@Override
		public boolean isDirty() {
			return isDirty;
		}


	}
		

	
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
		 * Returns a shallow copy of this segment with deep copied
		 * positions {@link #p0} and {@link #p1}.
		 * @return copy of this segment
		 */
		public SegmentDetails copy() {
			SegmentDetails clone = this.clone();
			clone.p0 = Utils.copy(clone.p0);
			clone.p1 = Utils.copy(clone.p1);
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
	
	/**
	 * The PDFRenderer interface defines the method
	 * {@link #renderPDF(PDDocument, PDPage, int, int, int, int)}
	 * which 'renders' the PDFRenderers's content as pdf objects
	 * in content streams, i.e. fills content streams with elements and appends them to the specified page.
	 *
	 */
	public interface PDFRenderer {
	    /**
	     * Renders this PDFRenderers contents, that is creating
	     * pdf elements and appending them to the specified page
	     * within the corresponding document.
	     *
	     * @param doc the PDF document holding the page
	     * @param page page in pdf doc to which elements are to be appended
	     * @param x x coordinate of the current viewport
	     * @param y y coordinate of the current viewport
	     * @param w width of the current viewport
	     * @param h height of the current viewport
	     */
	    public default void renderPDF(PDDocument doc, PDPage page, int x, int y, int w, int h){}
	}

	
	/**
	 * The Renderer interface defines methods to 
	 * initialize the renderer,
	 * execute a rendering pass,
	 * close the renderer.
	 * <p>
	 * <b>Implementation Notice:</b><br>
	 * If this renderer directly uses a shader,
	 * its fragment shader is obliged to output color for two
	 * buffers, which are the two color attachments of an {@link FBO}.
	 * These have to be written to <br>
	 * {@code layout(location=0) out vec4 c1;} and <br>
	 * {@code layout(location=1) out vec4 c2;}.<br>
	 * When the renderer has no use for the picking attachment, {@code vec4(0,0,0,0)}
	 * can be written as default.
	 * 
	 * @author hageldave
	 */
	public interface Renderer extends PDFRenderer {
		/**
		 * Renders this {@link Renderer}'s 'scene'.
		 * This is the fallback path in case OpenGL based rendering through {@link #render(int, int, int, int)}
		 * is not available. 
		 * @param g main graphics object for drawing onto the framebuffer
		 * @param p graphics object for drawing onto the picking framebuffer (invisible but used for picking)
		 * @param w width of the current viewport in pixels
		 * @param h height of the current viewport in pixels
		 */
		public default void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {};
		
		/**
		 * Whether this renderer is enabled or not. By default a renderer is enabled and will
		 * render upon {@link #render(int, int, int, int)} or {@link #renderSVG(org.w3c.dom.Document, org.w3c.dom.Element, int, int)}.
		 * When disabled those methods return right away and will not render anything.
		 * @return true when active
		 */
		public boolean isEnabled();
	
	}
	
	public abstract class GenericRenderer<T extends Renderable> implements Renderer {
		protected LinkedList<T> itemsToRender = new LinkedList<>();
		protected Rectangle2D view = null;
		protected boolean isEnabled = true;
		
		/**
		 * Adds an item to this renderer's {@link #itemsToRender} list.
		 * The renderer will take care of calling {@link Renderable#initGL()} during
		 * its {@link #render(int, int, int, int)} method and will as well call
		 * {@link Renderable#updateGL(boolean)} if {@link Renderable#isDirty()}.
		 * 
		 * @param item to add
		 * @return this for chaining
		 */
		public GenericRenderer<T> addItemToRender(T item){
			if(!itemsToRender.contains(item))
				itemsToRender.add(item);
			return this;
		}
		
		/**
		 * Removes an item from this renderer's {@link #itemsToRender} list.
		 * @param item to remove
		 * @return true when successfully removed, else false (e.g. when not contained in list)
		 */
		public boolean removeItemToRender(T item){
			return itemsToRender.remove(item);
		}
		
		@Override
		public boolean isEnabled() {
			return isEnabled;
		}

		/**
		 * @return the list of items to render.
		 */
		public LinkedList<T> getItemsToRender() {
			return itemsToRender;
		}
	
		public void setView(Rectangle2D view){
			this.view = Objects.isNull(view) ? null:Utils.copy(view);
		}



	}
	
	/**
	 * The LinesRenderer is an implementation of the {@link GenericRenderer}
	 * for {@link Lines}.
	 * This renderer uses a geometry shader that extends a line primitive
	 * into a quad of width that corresponds to the line width of the Lines
	 * object.
	 * <br>
	 * Its fragment shader draws the picking color into the second render buffer
	 * alongside the 'visible' color that is drawn into the first render buffer.
	 *
	 * @author hageldave
	 */
	public class LinesRenderer extends GenericRenderer<Lines> {

		protected static final char NL = '\n';
	}
	
	/**
	 * The TrianglesRenderer is an implementation of the {@link GenericRenderer}
	 * for {@link Text}.
	 * It draws the vertex arrays of its Text objects and uses the texture of the
	 * {@link CharacterAtlas} corresponding to the Text's font to texture
	 * the drawn quads in order to display text.
	 * <br>
	 * Its fragment shader draws the picking color into the second render buffer
	 * alongside the 'visible' color that is drawn into the first render buffer.
	 * 
	 * @author hageldave
	 */
	public class TextRenderer extends GenericRenderer<Text> {

		protected static final char NL = '\n';
	}

	/**
	 * The TickMarkGenerator interface provides the
	 * {@link #genTicksAndLabels(double, double, int, boolean)} method.
	 * It is used by the {@link CoordSysRenderer} to obtain tick marks and
	 * labels for its current view of coordinates.
	 * 
	 * @author hageldave
	 */
	public interface TickMarkGenerator {

		/**
		 * Generates a number of tick marks and corresponding labels.
		 * The first entry of the returned pair are the tick mark values,
		 * the second part are the corresponding labels.
		 * 
		 * @param min minimum of value range for which ticks marks are to be generated
		 * @param max maximum of value range for which ticks marks are to be generated
		 * @param desiredNumTicks the desired number of generated tick marks, not obligatory,
		 * can also create less or more tick marks if that leads to better tick values.
		 * @param verticalAxis true if marks are for vertical axis, false when for horizontal axis
		 * @return pair of a tick mark value array and corresponding label array for these values
		 */
		public Pair<double[], String[]> genTicksAndLabels(double min, double max, int desiredNumTicks, boolean verticalAxis);
		
		
		public static final TickMarkGenerator NO_TICKS = (double min, double max, int desiredNumTicks, boolean verticalAxis)->Pair.of(new double[0], new String[0]);
		
	}
	
	/**
	 * Implementation of the extended Wilkinson algorithm for tick label positioning.
	 * See <a href="http://vis.stanford.edu/papers/tick-labels">vis.stanford.edu/papers/tick-labels</a> for details.
	 * <p>
	 * The algorithm uses set of 'nice' value increments which are order by
	 * 'niceness', i.e. earlier increment values are to be preferred over the later ones.
	 * The algorithm tries to figure out which increments to use at what scale in order to
	 * fill a desired value range with ticks.
	 * It utilizes a scoring function to decide which tick marks to use.
	 * <p>
	 * By default the ordered set of nice increments is {1, 5, 2, 2.5, 4, 3, 1.5, 6, 8}.
	 * This can be changed by sub classing this class and setting the {@link #Q} array
	 * accordingly.
	 * <p>
	 * There is also a set of weights used to calculate the score of a tick mark sequence 
	 * based on the qualities: simplicity, coverage, density and legibility (legibility 
	 * calculation is not implemented).
	 * The influence of these qualities on the score is determined by the weights in the
	 * attribute {@link #w}. 
	 * 
	 * @author hageldave
	 */
	public static class ExtendedWilkinson implements TickMarkGenerator {

		/** preference-decreasing ordered set of nice increments */
		protected double[] Q = new double[]{1, 5, 2, 2.5, 4, 3, 1.5, 6, 8};
		/** weights for simplicity, coverage, density and legibility */
		protected double[] w = new double[]{0.2, 0.25, 0.5, 0.05};

		/**
		 * Uses either decimal or scientific notation for the labels and strips unnecessary
		 * trailing zeros.
		 * If one of the specified tick values is formatted in scientific notation by the
		 * {@link String#format(String, Object...)} '%g' option, all values will use scientific
		 * notation, otherwise decimal is used.
		 * 
		 * @param ticks to be labeled
		 * @return String[] of labels corresponding to specified tick values
		 */
		protected String[] labelsForTicks(double[] ticks){
			String str1 = String.format(Locale.US, "%g", ticks[0]);
			String str2 = String.format(Locale.US, "%g", ticks[ticks.length-1]);
			String[] labels = new String[ticks.length];
			if(str1.contains("e") || str2.contains("e")){
				for(int i=0; i<ticks.length; i++){
					String l = String.format(Locale.US, "%e", ticks[i]);
					String[] Esplit = l.split("e", -2);
					String[] dotsplit = Esplit[0].split("\\.",-2);
					dotsplit[1] = ('#'+dotsplit[1])
							.replaceAll("0", " ")
							.trim()
							.replaceAll(" ", "0")
							.replaceAll("#", "");
					dotsplit[1] = dotsplit[1].isEmpty() ? "0":dotsplit[1];
					l = dotsplit[0]+'.'+dotsplit[1]+'e'+Esplit[1];
					labels[i] = l;
				}
			} else {
				for(int i=0; i<ticks.length; i++){
					String l = String.format(Locale.US, "%f", ticks[i]);
					if(l.contains(".")){
						String[] dotsplit = l.split("\\.",-2);
						dotsplit[1] = ('#'+dotsplit[1])
								.replaceAll("0", " ")
								.trim()
								.replaceAll(" ", "0")
								.replaceAll("#", "");
						if(dotsplit[1].isEmpty()){
							l = dotsplit[0];
						} else {
							l = dotsplit[0]+'.'+dotsplit[1];
						}
					}
					labels[i] = l;
				}
			}
			return labels;
		}


		@Override
		public Pair<double[], String[]> genTicksAndLabels(double min, double max, int desiredNumTicks,
				boolean verticalAxis) {
			double[] ticks = getTicks(min, max, desiredNumTicks, this.Q, this.w);
			String[] labelsForTicks = labelsForTicks(ticks);
			return new Pair<double[], String[]>(ticks, labelsForTicks);
		}


		/* STATIC DOWN HERE */


		static double coverage(double dmin, double dmax, double lmin, double lmax){
			return 1 - 0.5 * (Math.pow(dmax - lmax,2) + Math.pow(dmin - lmin,2)) / Math.pow(0.1 * (dmax - dmin), 2);
		}

		static double coverage_max(double dmin, double dmax, double span){
			double drange = dmax - dmin;
			if(span > drange){
				return 1 - Math.pow(0.5 * (span - drange),2) / Math.pow(0.1 * drange,2);
			}
			return 1;
		}

		static double density(double k, double m, double dmin, double dmax, double lmin, double lmax){
			double r = (k - 1) / (lmax - lmin);
			double rt = (m - 1) / (Math.max(lmax, dmax) - Math.min(lmin, dmin));
			return 2 - Math.max(r / rt, rt / r);
		}

		static double density_max(double k, double m){
			if(k >= m){
				return 2 - (k - 1.0) / (m - 1.0);
			}
			return 1;
		}

		static double simplicity(double q, double[] Q, double j, double lmin, double lmax, double lstep){
			double eps = 1e-10;
			int n = Q.length;
			int i = Arrays.binarySearch(Q, q) + 1;
			int v = 0;
			if( ((lmin % lstep) < eps) || ((((lstep - lmin) % lstep) < eps) && (lmin <= 0) && (lmax >= 0)) ){
				v = 1;
			} else {
				v = 0;
			}
			return (n - i) / (n - 1.0) + v - j;
		}

		static double simplicity_max(double q, double[] Q, int j){
			int n = Q.length;
			int i = Arrays.binarySearch(Q, q) + 1;
			int v = 1;
			return (n - i) / (n - 1.0) + v - j;
		}

		static double legibility(double lmin, double lmax, double lstep){
			return 1;
		}

		static double score(double[] weights, double simplicity, double coverage, double density, double legibility){
			return weights[0] * simplicity + weights[1] * coverage + weights[2] * density + weights[3] * legibility;
		}

		static double[] ext_wilk(double dmin, double dmax, int m, int onlyInside, double[] Q, double[] w){
			if(dmin >= dmax || m < 1){
				return new double[]{dmin, dmax, dmax-dmin,1,0,2,0};
			}
			double best_score = -1.0;
			double[] result = null;

			int j = 1;
			while(j < 5){
				for(double q:Q){
					double sm = simplicity_max(q, Q, j);
					if(score(w, sm,  1, 1, 1) < best_score){
						j = Integer.MAX_VALUE-1;
						break;
					}
					int k = 2;
					while(k <= m+2){
						double dm = density_max(k, m);
						if(score(w, sm, 1, dm, 1) < best_score){
							break;
						}
						double delta = (dmax - dmin) / (k + 1.) / j / q;
						int z = (int)Math.ceil(Math.log10(delta));
						int zmax = z+3;
						while(z < zmax){
							double step = j*q*Math.pow(10,z);
							double cm = coverage_max(dmin, dmax, step*(k-1));
							if(score(w, sm, cm, dm, 1) < best_score){
								break;
							}

							double min_start = Math.floor(dmax / step) * j - (k - 1) * j;
							double max_start = Math.ceil(dmin / step) * j;

							if(min_start > max_start || /*precision insanity check*/(min_start+1)==min_start){
								z++;
								break;
							}
							for(double start=min_start; start<max_start+1; start++){
								double lmin = start * (step/j);
								double lmax = lmin + step * (k-1);
								double lstep = step;

								double s = simplicity(q, Q, j, lmin, lmax, lstep);
								double c = coverage(dmin, dmax, lmin, lmax);
								double d = density(k, m, dmin, dmax, lmin, lmax);
								double l = legibility(lmin, lmax, lstep);
								double scr = score(w, s,c,d,l);
								if( 	scr > best_score 
										&& 
										(onlyInside <= 0 || (lmin >= dmin && lmax <= dmax))
										&&
										(onlyInside >= 0 || (lmin <= dmin && lmax >= dmax))
										){
									best_score = scr;
									result = new double[]{lmin, lmax, lstep, j, q, k, scr};
								}
							}
							z++;
						}
						k++;
					}
				}
				j++;
			}
			return result;
		}

		static double[] getTicks(double dmin, double dmax, int m, double[] Q, double[] w){
			double[] l = ext_wilk(dmin, dmax, m, 1, Q, w);
			double lmin  = l[0];
			//double lmax  = l[1];
			double lstep = l[2];
			//int    j =(int)l[3];
			//double q     = l[4];
			int    k =(int)l[5];
			//double scr   = l[6];

			double[] ticks = new double[k];
			for(int i=0; i < k; i++){
				ticks[i] = lmin + i*lstep;
			}
			return ticks;
		}

	}



	
	/**
	 * The CoordSysRenderer is a {@link Renderer} that displays a coordinate system.
	 * This coordinate system is enclosed by 4 axes that form a rectangle around the
	 * area that displays the contents of the coordinate system.
	 * <p>
	 * The upper x-axis and right y-axis feature the labels (names) of the axes.
	 * The lower x-axis and the left y-axis feature tick marks and labels that
	 * help to orientate and read off coordinates.
	 * The positioning and labeling of the tick marks is done by a {@link TickMarkGenerator}
	 * which is per default an instance of {@link ExtendedWilkinson}.
	 * For each tick a vertical or horizontal guide line is drawn across the area of the
	 * coordinate system.
	 * <p>
	 * What coordinate range the coordinate system area corresponds to is controlled by
	 * the coordinate view (see {@link #setCoordinateView(double, double, double, double)})
	 * and defaults to [-1,1] for both axes.
	 * The contents that are drawn inside the coordinate area are rendered by the content renderer
	 * (see {@link #setContent(Renderer)}).
	 * If that renderer implements the {@link AdaptableView} interface it will be passed the
	 * view matrix corresponding to the coordinate view.
	 * The content renderer will be able to draw within the viewport defined by the coordinate
	 * system area of this CoordSysRenderer.
	 * <p>
	 * Optionally a {@link Renderer} for drawing a legend (such as the {@link Legend} class)
	 * can be set to either the bottom or right hand side of the coordinate system (can also
	 * use both areas at once).
	 * Use {@link #setLegendBottom(Renderer)} or {@link #setLegendRight(Renderer)} to do so.
	 * The legend area size can be partially controlled by {@link #setLegendBottomHeight(int)}
	 * and {@link #setLegendRightWidth(int)} if this is needed.
	 * <p>
	 * The overlay renderer ({@link #setOverlay(Renderer)}) can be used to finally draw over all
	 * of the renderer viewport.
	 * <p>
	 * For interacting with this {@link CoordSysRenderer} there already exist implementations of MouseListeners
	 * for panning and zooming (see {@link CoordSysPanning} and {@link CoordSysScrollZoom}).
	 * 
	 * @author hageldave
	 */
	public class CoordSysRenderer implements Renderer {
		protected LinesRenderer preContentLinesR = new LinesRenderer();
		protected TextRenderer preContentTextR = new TextRenderer();
		protected LinesRenderer postContentLinesR = new LinesRenderer();
		protected TextRenderer postContentTextR = new TextRenderer();
		protected Renderer overlay;
		protected Renderer content=null;
		protected Renderer legendRight=null;
		protected Renderer legendBottom=null;
		
		protected int legendRightWidth = 70;
		protected int legendBottomHeight = 20;
		
		//@GLCoordinates
		protected Rectangle legendRightViewPort = new Rectangle();
		//@GLCoordinates
		protected Rectangle legendBottomViewPort = new Rectangle();
		
		//@GLCoordinates
		protected Rectangle currentViewPort = new Rectangle();

		
		protected Rectangle2D coordinateView = new Rectangle2D.Double(-1,-1,2,2);

		protected TickMarkGenerator tickMarkGenerator = new ExtendedWilkinson();

		protected Lines axes = new Lines().setVertexRoundingEnabled(true);
		protected Lines ticks = new Lines().setVertexRoundingEnabled(true);
		protected Lines guides = new Lines().setVertexRoundingEnabled(true);
		protected LinkedList<Text> tickMarkLabels = new LinkedList<>();
		protected Text xAxisLabelText = new Text("", 13, Font.PLAIN);
		protected Text yAxisLabelText = new Text("", 13, Font.PLAIN);

		protected double[] xticks;
		protected double[] yticks;

		protected int viewportwidth=0;
		protected int viewportheight=0;
		protected boolean isDirty = true;

		protected IntSupplier tickColor;
		protected IntSupplier guideColor;
		protected IntSupplier textColor;

		protected int paddingLeft = 10;
		protected int paddingRight = 10;
		protected int paddingTop = 10;
		protected int paddingBot = 10;
		
		//@GLCoordinates
		protected PointeredPoint2D coordsysAreaLB = new PointeredPoint2D();
		//@GLCoordinates
		protected PointeredPoint2D coordsysAreaRT = Utils.copy(coordsysAreaLB);
		//@GLCoordinates
		protected PointeredPoint2D coordsysAreaLT = new PointeredPoint2D(coordsysAreaLB.x, coordsysAreaRT.y);
		//@GLCoordinates
		protected PointeredPoint2D coordsysAreaRB = new PointeredPoint2D(coordsysAreaRT.x, coordsysAreaLB.y);

		
		protected String xAxisLabel = null;
		protected String yAxisLabel = null;
		
		protected ActionListener coordviewListener;
		protected boolean isEnabled=true;

		protected ColorScheme colorScheme;
		
		/**
		 * Sets up a CoordSysRenderer with the default color scheme
		 */
		public CoordSysRenderer() {
			this.colorScheme = DefaultColorScheme.LIGHT.get();
			setupCoordSysRenderer();
		}
		
		/**
		 * Helper method to setup the CoordSysRenderer
		 */
		protected void setupCoordSysRenderer() {
			this.axes.addSegment(coordsysAreaLB, coordsysAreaRB).setColor(()->getColorScheme().getColor1());
			this.axes.addSegment(coordsysAreaLB, coordsysAreaLT).setColor(()->getColorScheme().getColor1());
			this.axes.addSegment(coordsysAreaLT, coordsysAreaRT).setColor(()->getColorScheme().getColor2());
			this.axes.addSegment(coordsysAreaRB, coordsysAreaRT).setColor(()->getColorScheme().getColor2());
			this.axes.setGlobalThicknessMultiplier(2);
			
			this.preContentLinesR
			.addItemToRender(guides)
			.addItemToRender(ticks);
			this.preContentTextR
			.addItemToRender(xAxisLabelText)
			.addItemToRender(yAxisLabelText);
			this.postContentLinesR.addItemToRender(axes);
			
			this.guideColor = ()->getColorScheme().getColor4();
			this.tickColor = ()->getColorScheme().getColor3();
			this.textColor = ()->getColorScheme().getColorText();

			updateColors();
		}
		
		/**
		 * Helper method to update the colors if the color scheme is changed.
		 * @return this for chaining
		 */
		protected CoordSysRenderer updateColors() {
			// axes already use a pointer to color scheme and need only to be set dirty
			this.axes.setDirty();
			
			this.xAxisLabelText.setColor(this.textColor.getAsInt());
			this.yAxisLabelText.setColor(this.textColor.getAsInt());
			
			updateLegendColorScheme(legendBottom);
			updateLegendColorScheme(legendRight);
			
			setDirty();
			return this;
		}

		/**
		 * Sets the {@link #isDirty} state of this CoordSysRenderer to true.
		 * This indicates that axis locations, tick marks, labels and guides
		 * have to be recomputed.
		 * The recomputing will be done during {@link #render(int, int, int, int)} which
		 * will set the isDirty state back to false.
		 */
		public void setDirty() {
			this.isDirty = true;
		}

		/**
		 * @param legend color scheme of the legend will be updated if it is from type {@link Legend}
		 */
		protected void updateLegendColorScheme(final Renderer legend) {
			if (legend instanceof Legend) {
				((Legend) legend).setColorScheme(getColorScheme());
			}
		}
		
		/**
		 * @return the {@link ColorScheme} of the CoordSysRenderer.
		 */
		public ColorScheme getColorScheme() {
			return colorScheme;
		}
		
		/**
		 * Sets up pretty much everything.
		 * <ul>
		 * <li>the bounds of the coordinate system frame ({@link #coordsysAreaLB}, {@link #coordsysAreaRT})</li>
		 * <li>the tick mark values and labels</li>
		 * <li>the tick mark guides</li>
		 * <li>the location of the axis labels</li>
		 * <li>the areas for the legends (right and bottom legend)</li>
		 * </ul>
		 */
		protected void setupAndLayout() {
			Pair<double[],String[]> xticksAndLabels = tickMarkGenerator.genTicksAndLabels(
					coordinateView.getMinX(), 
					coordinateView.getMaxX(), 
					5, 
					false);
			Pair<double[],String[]> yticksAndLabels = tickMarkGenerator.genTicksAndLabels(
					coordinateView.getMinY(), 
					coordinateView.getMaxY(), 
					5, 
					true);
			this.xticks = xticksAndLabels.first;
			this.yticks = yticksAndLabels.first;
			String[] xticklabels = xticksAndLabels.second;
			String[] yticklabels = yticksAndLabels.second;

			final int tickfontSize = 11;
			final int labelfontSize = 12;
			final int style = Font.PLAIN;
			// find maximum length of y axis labels
			int maxYTickLabelWidth = 0;
			for(String label:yticklabels){
				int labelW = CharacterAtlas.boundsForText(label.length(), tickfontSize, style).getBounds().width;
				maxYTickLabelWidth = Math.max(maxYTickLabelWidth, labelW);
			}
			int maxXTickLabelHeight = CharacterAtlas.boundsForText(1, tickfontSize, style).getBounds().height;
			int maxLabelHeight = CharacterAtlas.boundsForText(1, labelfontSize, style).getBounds().height;

			int legendRightW = Objects.nonNull(legendRight) ? legendRightWidth+4:0;
			int legendBotH = Objects.nonNull(legendBottom) ? legendBottomHeight+4:0;

			// move coordwindow origin so that labels have enough display space
			coordsysAreaLB.x[0] = maxYTickLabelWidth + paddingLeft + 7;
			coordsysAreaLB.y[0] = maxXTickLabelHeight + paddingBot + legendBotH + 6;
			// move opposing corner of coordwindow to have enough display space
			coordsysAreaRT.x[0] = viewportwidth-paddingRight-maxLabelHeight-legendRightW-4;
			coordsysAreaRT.y[0] = viewportheight-paddingTop-maxLabelHeight-4;

			// dispose of old stuff
			ticks.removeAllSegments();
			guides.removeAllSegments();
			for(Text txt:tickMarkLabels){
				preContentTextR.removeItemToRender(txt);
				txt.close();
			}
			tickMarkLabels.clear();

			// create new stuff
			double xAxisWidth = coordsysAreaLB.distance(coordsysAreaRB);
			double yAxisHeight = coordsysAreaLB.distance(coordsysAreaLT);
			// xaxis ticks
			for(int i=0; i<xticks.length; i++){
				// tick
				double m = (xticks[i]-coordinateView.getMinX())/coordinateView.getWidth();
				double x = coordsysAreaLB.getX()+m*xAxisWidth;
				Point2D onaxis = new Point2D.Double(Math.round(x),coordsysAreaLB.getY());
				ticks.addSegment(onaxis, new TranslatedPoint2D(onaxis, 0,-4)).setColor(tickColor);
				// label
				Text label = new Text(xticklabels[i], tickfontSize, style, this.textColor.getAsInt());
				Dimension textSize = label.getTextSize();
				label.setOrigin(new Point2D.Double(
						(int)(onaxis.getX()-textSize.getWidth()/2.0), 
						(int)(onaxis.getY()-6-textSize.getHeight())+0.5));
				tickMarkLabels.add(label);
				// guide
				guides.addSegment(onaxis, new TranslatedPoint2D(onaxis, 0, yAxisHeight)).setColor(guideColor);
			}
			// yaxis ticks
			for(int i=0; i<yticks.length; i++){
				// tick
				double m = (yticks[i]-coordinateView.getMinY())/coordinateView.getHeight();
				double y = m*yAxisHeight;
				Point2D onaxis = new TranslatedPoint2D(coordsysAreaLB, 0, Math.round(y));
				ticks.addSegment(onaxis, new TranslatedPoint2D(onaxis, -4, 0)).setColor(tickColor);
				// label
				Text label = new Text(yticklabels[i], tickfontSize, style, this.textColor.getAsInt());
				Dimension textSize = label.getTextSize();
				label.setOrigin(new TranslatedPoint2D(onaxis, -7-textSize.getWidth(), -Math.round(textSize.getHeight()/2.0)+0.5));
				tickMarkLabels.add(label);
				// guide
				guides.addSegment(onaxis, new TranslatedPoint2D(onaxis, xAxisWidth, 0)).setColor(guideColor);
			}
			for(Text txt: tickMarkLabels){
				preContentTextR.addItemToRender(txt);
			}
			// axis labels
			xAxisLabelText.setTextString(getxAxisLabel());
			xAxisLabelText.setOrigin(new TranslatedPoint2D(coordsysAreaLT, xAxisWidth/2 - xAxisLabelText.getTextSize().width/2, 4));
			yAxisLabelText.setTextString(getyAxisLabel());
			yAxisLabelText.setAngle(-(float)Math.PI/2);
			yAxisLabelText.setOrigin(new TranslatedPoint2D(coordsysAreaRB, 4, yAxisHeight/2 + yAxisLabelText.getTextSize().width/2));

			// setup legend areas
			if(Objects.nonNull(legendRight)){
				legendRightViewPort.setBounds(
						(int)(yAxisLabelText.getOrigin().getX()+yAxisLabelText.getTextSize().getHeight()+4), 
						paddingBot, 
						legendRightWidth, 
						(int)(coordsysAreaRT.getY()-paddingBot)
						);
			} else {
				legendRightViewPort.setBounds(0, 0, 0, 0);
			}
			if(Objects.nonNull(legendBottom)){
				legendBottomViewPort.setBounds(
						(int)coordsysAreaLB.getX(),
						paddingBot,
						(int)(coordsysAreaLB.distance(coordsysAreaRB)),
						legendBottomHeight
						);
			} else {
				legendBottomViewPort.setBounds(0, 0, 0, 0);
			}
		}


		@Override
		public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
			if(!isEnabled()){
				return;
			}
			currentViewPort.setRect(0, 0, w, h);
			if(isDirty || viewportwidth != w || viewportheight != h){
				// update axes
				axes.setDirty();
				viewportwidth = w;
				viewportheight = h;
				setupAndLayout();
				isDirty = false;
			}
			preContentLinesR.renderFallback(g, p, w, h);
			preContentTextR.renderFallback(g, p, w, h);
			if(content != null){
				int viewPortX = (int)coordsysAreaLB.getX();
				int viewPortY = (int)coordsysAreaLB.getY();
				int viewPortW = (int)coordsysAreaLB.distance(coordsysAreaRB);
				int viewPortH = (int)coordsysAreaLB.distance(coordsysAreaLT);
				/*if(content instanceof AdaptableView){
					((AdaptableView) content).setView(coordinateView);
				}*/
				// create viewport graphics
				Graphics2D g_ = (Graphics2D)g.create(viewPortX, viewPortY, viewPortW, viewPortH);
				Graphics2D p_ = (Graphics2D)p.create(viewPortX, viewPortY, viewPortW, viewPortH);
				content.renderFallback(g_, p_, viewPortW, viewPortH);
			}
			postContentLinesR.renderFallback(g, p, w, h);
			postContentTextR.renderFallback(g, p, w, h);
			// draw legends
			if(Objects.nonNull(legendRight)){
				// create viewport graphics
				Graphics2D g_ = (Graphics2D)g.create(legendRightViewPort.x, legendRightViewPort.y, legendRightViewPort.width, legendRightViewPort.height);
				Graphics2D p_ = (Graphics2D)p.create(legendRightViewPort.x, legendRightViewPort.y, legendRightViewPort.width, legendRightViewPort.height);
				legendRight.renderFallback(g_, p_, legendRightViewPort.width, legendRightViewPort.height);
			}
			if(Objects.nonNull(legendBottom)){
				// create viewport graphics
				Graphics2D g_ = (Graphics2D)g.create(legendBottomViewPort.x, legendBottomViewPort.y, legendBottomViewPort.width, legendBottomViewPort.height);
				Graphics2D p_ = (Graphics2D)p.create(legendBottomViewPort.x, legendBottomViewPort.y, legendBottomViewPort.width, legendBottomViewPort.height);
				legendBottom.renderFallback(g_, p_, legendBottomViewPort.width, legendBottomViewPort.height);
			}
			
			// draw overlay
			if(Objects.nonNull(overlay)){
				overlay.renderFallback(g, p, w, h);
			}
		}
		
		@Override
		public void renderPDF(PDDocument doc, PDPage page, int x, int y, int w, int h) {
			if(!isEnabled()){
				return;
			}
			preContentLinesR.renderPDF(doc, page, x, y, w, h);
			preContentTextR.renderPDF(doc, page, x,y, w, h);
			if(content != null){
				int viewPortX = (int)(coordsysAreaLB.getX()+x);
				int viewPortY = (int)(coordsysAreaLB.getY()+y);
				int viewPortW = (int)coordsysAreaLB.distance(coordsysAreaRB);
				int viewPortH = (int)coordsysAreaLB.distance(coordsysAreaLT);
				/*if(content instanceof AdaptableView){
					((AdaptableView) content).setView(coordinateView);
				}*/
				// render the content into the group
				content.renderPDF(doc, page, viewPortX, viewPortY, viewPortW, viewPortH);
			}
			postContentLinesR.renderPDF(doc, page, x, y, w, h);
			postContentTextR.renderPDF(doc, page, x, y, w, h);
			if(Objects.nonNull(legendRight)){
				legendRight.renderPDF(doc, page, legendRightViewPort.x, legendRightViewPort.y, legendRightViewPort.width, legendRightViewPort.height);
			}
			if(Objects.nonNull(legendBottom)){
				legendBottom.renderPDF(doc, page, legendBottomViewPort.x, legendBottomViewPort.y, legendBottomViewPort.width, legendBottomViewPort.height);
			}
		}

		@Override
		public boolean isEnabled() {
			return this.isEnabled;
		}
		
		/**
		 * @return "X" if {@link #xAxisLabel} is null or the actual axis label.
		 */
		public String getxAxisLabel() {
			return xAxisLabel == null ? "X":xAxisLabel;
		}

		/**
		 * @return "Y" if {@link #yAxisLabel} is null or the actual axis label.
		 */
		public String getyAxisLabel() {
			return yAxisLabel == null ? "Y":yAxisLabel;
		}



	}

	/**
	 * Abstract class for {@link Renderable}s representing text that can be rendered using the
	 * {@link TextRenderer}.
	 * A text object describes a line of characters together with the following attributes:
	 * <ul>
	 * <li>fontsize (e.g. 12 pts.)</li>
	 * <li>font style (e.g. Font{@link Font#BOLD})</li>
	 * <li>color</li>
	 * <li>origin - the bottom left corner of the rectangle enclosing the text</li>
	 * <li>angle - the rotation of the text (around origin)</li>
	 * <li>picking color - the picking color with which the text is rendered into the (invisible) picking color attachment
	 * of an {@link FBO}. This color may serve as an identifier of the object that can be queried from a location of the
	 * rendering canvas. It may take on a value in range of 0xff000001 to 0xffffffff (16.777.214 possible values) or 0.
	 * </li>
	 * </ul>
	 * 
	 * @author hageldave
	 */
	public class Text implements Renderable {
		public final int fontsize; 
		public final int style;
		protected Dimension textSize;
		protected Color color;
		protected Color background = new Color(0, true);
		protected int pickColor;
		protected Point2D origin;
		//protected VertexArray va=null;
		protected float angle=0;
		protected String txtStr;
		protected boolean isDirty=true;
		protected boolean hidden=false;
		
		/**
		 * Creates a new Text object with the specified string and font configuration.
		 * @param textstr the text to be displayed
		 * @param fontsize point size of the font
		 * @param style of the font - one of {@link Font#PLAIN}, {@link Font#BOLD}, {@link Font#ITALIC}
		 * or bitwise union BOLD|ITALIC.
		 * @param textcolor color of the text
		 */
		public Text(String textstr, int fontsize, int style, Color textcolor) {
			this.txtStr = textstr;
			this.textSize = CharacterAtlas.boundsForText(textstr.length(), fontsize, style).getBounds().getSize();
			this.fontsize = fontsize;
			this.style = style;
			this.color = textcolor;
			this.origin = new Point(0, 0);
		}
		
		/**
		 * Creates a new Text object with the specified string and font configuration.
		 * @param textstr the text to be displayed
		 * @param fontsize point size of the font
		 * @param style of the font - one of {@link Font#PLAIN}, {@link Font#BOLD}, {@link Font#ITALIC}
		 * or bitwise union BOLD|ITALIC.
		 * @param textcolor color of the text (integer packed ARGB)
		 */
		public Text(String textstr, int fontsize, int style, int textcolor) {
			this(textstr,fontsize,style, new Color(textcolor, true));
		}

		/**
		 * Creates a new Text object with the specified string and font configuration.
		 * @param textstr the text to be displayed
		 * @param fontsize point size of the font
		 * @param style of the font - one of {@link Font#PLAIN}, {@link Font#BOLD}, {@link Font#ITALIC}
		 * or bitwise union BOLD|ITALIC.
		 */
		public Text(String textstr, int fontsize, int style) {
			this(textstr,fontsize,style, new Color(96, 96, 96));
		}
		
		/**
		 * Sets the string of this text.
		 * Only characters that are ASCII printable (more precisely ASCII characters [32..126]) will be
		 * displayed, other characters are mapped to whitespace for rendering.
		 * This set the {@link #isDirty()} state of this {@link Renderable} to true.
		 * @param txtStr the text string this object should display.
		 * @return this for chaining
		 */
		public Text setTextString(String txtStr) {
			this.txtStr = txtStr;
			this.textSize = CharacterAtlas.boundsForText(txtStr.length(), fontsize, style).getBounds().getSize();
			return setDirty();
		}
		
		@Override
		public boolean intersects(Rectangle2D rect) {
			if(getAngle()==0){
				Rectangle2D bounds = getBounds();
				return rect.intersects(bounds) || bounds.intersects(rect);
			} else {
				Rectangle2D bounds = getBoundsWithRotation();
				return rect.intersects(bounds) || bounds.intersects(rect);
			}
		}

		/**
		 * Sets the {@link #isDirty()} state of this renderable to true.
		 * This indicates that an {@link #updateGL(boolean)} call is necessary to sync GL resources.
		 * @return this for chaining
		 */
		public Text setDirty() {
			this.isDirty = true;
			return this;
		}
		
		@Override
		public boolean isDirty() {
			return isDirty;
		}

		
		/**
		 * disposes of the GL resources of this text object,
		 * i.e deletes the vertex array.
		 */
		//@Override
		//@GLContextRequired
		public void close() {
			//if(Objects.nonNull(va)){
				//va.close();
				//va = null;
			//}
		}



		/**
		 * Sets the color of this text
		 * @param color to set
		 * @return this for chaining
		 */
		public Text setColor(Color color) {
			this.color = color;
			return this;
		}

		/**
		 * Sets the color of this text in integer packed ARGB format.
		 * e.g. 0xff00ff00 for opaque green.
		 * @param argb integer packed ARGB color value
		 * @return this for chaining
		 */
		public Text setColor(int argb) {
			return this.setColor(new Color(argb, true));
		}

		/**
		 * @return this text's color
		 */
		public Color getColor() {
			return color;
		}
		
		/**
		 * Sets the background color of the text, per default this is
		 * transparent black (0x00000000) which wont be visible.
		 * @param background color
		 */
		public void setBackground(Color background) {
			this.background = background;
		}

		/**
		 * Sets the background color of the text, per default this is
		 * transparent black (0x00000000) which wont be visible.
		 * @param argb integer packed ARGB color value
		 */
		public void setBackground(int argb) {
			this.background = new Color(argb, true);
		}
		
		public Color getBackground() {
			return background;
		}
		
		/**
		 * @return normalized red channel of this text's color (in [0,1])
		 */
		public float getColorR() {
			return color.getRed()/255f;
		}

		/**
		 * @return normalized green channel of this text's color (in [0,1])
		 */
		public float getColorG() {
			return color.getGreen()/255f;
		}

		/**
		 * @return normalized blue channel of this text's color (in [0,1])
		 */
		public float getColorB() {
			return color.getBlue()/255f;
		}

		/**
		 * @return normalized alpha channel of this text's color (in [0,1])
		 */
		public float getColorA() {
			return color.getAlpha()/255f;
		}
		
		/**
		 * Sets the picking color of this {@link Text} object. 
		 * The picking color is the color with which quads of the individual characters are rendered into the
		 * (invisible) picking color attachment of an {@link FBO}. 
		 * This color may serve as an identifier of the object that can be queried from a location of the
		 * rendering canvas. It may take on a value in range of 0xff000001 to 0xffffffff (16.777.214 possible values).
		 * @param pickColor opaque integer packed RGB value, 0 or one in [0xff000001..0xffffffff]. 
		 * When a transparent color is specified its alpha channel will be set to 0xff to make it opaque.
		 * @return this for chaining
		 */
		public Text setPickColor(int pickColor) {
			this.pickColor = pickColor;
			// can only use opaque colors cause transparent colors will not work on overlaps
			if(pickColor != 0)
				this.pickColor = pickColor | 0xff000000;
			return this;
		}

		/**
		 * @return the picking color of this {@link Text} object
		 */
		public int getPickColor() {
			return pickColor;
		}
		
		/**
		 * @return the normalized red channel of the picking color (in [0,1])
		 */
		public float getPickColorR() {
			return r(pickColor)/255f;
		}

		/**
		 * @return the normalized green channel of the picking color (in [0,1])
		 */
		public float getPickColorG() {
			return g(pickColor)/255f;
		}

		/**
		 * @return the normalized blue channel of the picking color (in [0,1])
		 */
		public float getPickColorB() {
			return b(pickColor)/255f;
		}
		
		/**
		 * @return the normalized alpha channel of the picking color (in [0,1])
		 */
		public float getPickColorA() {
			return a(pickColor)/255f;
		}

		/**
		 * @return the dimensions in pixels of this text object
		 */
		public Dimension getTextSize() {
			return textSize;
		}
		
		/**
		 * @return the bounding rectangle of this text
		 */
		public Rectangle2D getBounds() {
			return new Rectangle2D.Double(origin.getX(), origin.getY(), textSize.getWidth(), textSize.getHeight());
		}
		
		/**
		 * @return the bounding rectangle of this text with its rotation taken into account.
		 */
		public Rectangle2D getBoundsWithRotation() {
			Rectangle2D bounds = new Rectangle2D.Double(0, 0, getTextSize().width, getTextSize().height);
			AffineTransform transform = new AffineTransform();
			transform.translate(origin.getX(), origin.getY());
			transform.rotate(angle);
			return transform.createTransformedShape(bounds).getBounds2D();
		}

		/**
		 * @return the origin of this text object, i.e. the bottom left corner of the rectangle enclosing the text,
		 * the text's location so to say
		 */
		public Point2D getOrigin() {
			return origin;
		}

		/**
		 * Sets the origin of this text object, i.e. the bottom left corner of the rectangle enclosing the text,
		 * the text's location so to say
		 * @param origin to set
		 * @return this for chaining
		 */
		public Text setOrigin(Point2D origin) {
			this.origin = origin;
			return this;
		}

		/**
		 * Sets the origin of this text object, i.e. the bottom left corner of the rectangle enclosing the text,
		 * the text's location so to say
		 * @param x coordinate of origin
		 * @param y coordinate of origin
		 * @return this for chaining
		 */
		public Text setOrigin(int x, int y) {
			return this.setOrigin(new Point(x, y));
		}
		
		/**
		 * @return the rotation angle in radian by which this text object is rotated around its origin.
		 */
		public float getAngle() {
			return angle;
		}
		
		/**
		 * Sets the rotation angle in radian by which this text object is rotated around its origin.
		 * @param angle rotation angle
		 * @return this for chaining
		 */
		public Text setAngle(double angle) {
			this.angle = (float)angle;
			return this;
		}
		
		@Override
		public boolean isHidden() {
			return hidden;
		}
		
		/**
		 * Hides or unhides this Text object, i.e. sets the {@link #isHidden()} field
		 * value. When hidden, renderers will not draw it.
		 * @param hide true when hiding
		 * @return this for chaining
		 */
		public Text hide(boolean hide) {
			this.hidden = hide;
			return this;
		}
	}
	
	/**
	 * Pair class. 
	 * Access directly through {@link #first} and {@link #second}.
	 * 
	 * @author hageldave
	 *
	 * @param <T1> type 1
	 * @param <T2> type 2
	 */
	public static class Pair<T1,T2> {

		public final T1 first;
		
		public final T2 second;
		
		/**
		 * Creates a pair
		 * @param first part of pair
		 * @param second part of pair
		 */
		public Pair(T1 first, T2 second) {
			this.first = first;
			this.second = second;
		}
		
		/**
		 * syntactic sugar
		 * @param first part of pair
		 * @param second part of pair
		 * @return a new Pair
		 * 
		 * @param <T1> type 1
		 * @param <T2> type 2
		 */
		public static <T1,T2> Pair<T1, T2> of(T1 first, T2 second){
			return new Pair<>(first, second);
		}
		
		
		@Override
		public boolean equals(Object obj) {
			if(obj != null && obj instanceof Pair){
				Pair<?,?> other = (Pair<?,?>)obj;
				return Objects.equals(first, other.first) && Objects.equals(second, other.second);
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(first,second);
		}
		
		@Override
		public String toString() {
			return String.format("{%s, %s}", first,second);
		}
		
	}
	
	/**
	 * The TranslatedPoint2D is an implementation of {@link Point2D}
	 * that references another Point2D and has a certain fixed
	 * translation from that point.
	 * When changing the location of the referenced point, this
	 * point will change accordingly.
	 * 
	 * @author hageldave
	 */
	public class TranslatedPoint2D extends Point2D {
		
		public final double tx,ty;
		public final Point2D origin;
		
		/**
		 * Creates a new {@link TranslatedPoint2D} from the specified
		 * point with specified translation.
		 * @param origin the point from which the created point is translated
		 * @param xt translation in x direction
		 * @param yt translation in y direction
		 */
		public TranslatedPoint2D(Point2D origin, double xt, double yt) {
			this.tx = xt;
			this.ty = yt;
			this.origin = origin;
		}

		@Override
		public double getX() {
			return origin.getX()+tx;
		}

		@Override
		public double getY() {
			return origin.getY()+ty;
		}

		/**
		 * Sets the location of this point's {@link #origin} so that
		 * this point ends up at the desired location.
		 */
		@Override
		public void setLocation(double x, double y) {
			origin.setLocation(x-tx, y-ty);
		}
		
		@Override
		public TranslatedPoint2D clone() {
			return new TranslatedPoint2D(Utils.copy(origin), tx, ty);
		}
		
		@Override
		public String toString() {
			return getClass().getSimpleName()+"["+origin.getX()+(tx<0?"-":"+")+tx+"="+getX()+" ,"+origin.getY()+(ty<0?"-":"+")+ty+"="+getY()+"]";
		}

	}
	
	public interface ImgBase {
		

	}
	
	public class Img {
		/** data array of this Img containing a value for each pixel in row major order
		 * @since 1.0 */
		private int[] data;

		/** width and height of this image */
		private int width,height;
		
		/**
		 * Creates a new Img of specified dimensions.
		 * Values are initialized to 0.
		 * @param width of the Img
		 * @param height of the Img
		 * @since 1.0
		 */
		public Img(int width, int height){
			this.data = new int[width*height];
			this.width = width;
			this.height = height;
		}
		
		/**
		 * @return width of this Img
		 * @since 1.0
		 */
		public int getWidth(){
			return this.width;
		}

		/**
		 * @return height of this Img
		 * @since 1.0
		 */
		public int getHeight(){
			return this.height;
		}

		/**
		 * @return number of values (pixels) of this Img
		 * @since 1.0
		 */
		public int numValues(){
			return getWidth()*getHeight();
		}

		
		/**
		 * @return data array of this Img
		 * @since 1.0
		 */
		public int[] getData() {
			return data;
		}

		
		public Graphics2D createGraphics(){
			return getRemoteBufferedImage().createGraphics();
		}
		
		/**
		 * Creates a BufferedImage that shares the data of this Img. Changes in
		 * this Img are reflected in the created BufferedImage and vice versa.
		 * The created BufferedImage uses an ARGB DirectColorModel with an
		 * underlying DataBufferInt (similar to {@link BufferedImage#TYPE_INT_ARGB})
		 * @return BufferedImage sharing this Img's data.
		 * @see #createRemoteImg(BufferedImage)
		 * @see #toBufferedImage()
		 * @since 1.0
		 */
		public BufferedImage getRemoteBufferedImage(){
			DirectColorModel cm = new DirectColorModel(32,
					0x00ff0000,       // Red
	                0x0000ff00,       // Green
	                0x000000ff,       // Blue
	                0xff000000        // Alpha
	                );
			DataBufferInt buffer = new DataBufferInt(getData(), numValues());
			WritableRaster raster = Raster.createPackedRaster(buffer, getWidth(), getHeight(), getWidth(), cm.getMasks(), null);
			BufferedImage bimg = new BufferedImage(cm, raster, false, null);
			return bimg;
		}



	}
	
	/**
	 * The SignedDistanceCharacters class comprises signed distance fields 
	 * of a set of characters. 
	 * A signed distance field is an image in which each pixel encodes the
	 * distance to the nearest edge.
	 * Such an image of a character glyph allows for efficient rendering of
	 * text with arbitrary size and rotation using alpha testing and blending.
	 * For more details read this paper 
	 * <a href="https://dl.acm.org/citation.cfm?id=1281665">dl.acm.org/citation.cfm?id=1281665</a>.
	 * <p>
	 * The signed distance fields for each character are stored in a single
	 * image alongside a lookup table for the bounding rectangles for each
	 * character within that image.
	 * The set of characters used is defined in {@link #CHARACTERS} which is
	 * a String sorted by char value ascending.
	 * <p>
	 * Since the process of computing a SignedDistanceCharacters object is quite
	 * time consuming, there are pre-computed SignedDistanceCharacters instances
	 * for the Ubuntu Mono font used by JPlotter (see {@link FontProvider}) which
	 * are statically loaded from files.
	 * These are located in {@code .../resources/font/} alongside the corresponding
	 * true type font files.
	 * Similar to the {@link FontProvider#getUbuntuMono(float, int)}
	 * the {@link #getUbuntuMonoSDC(int)} method can be used to access the
	 * pre-computed SignedDistanceCharacters.
	 * 
	 * @author hageldave
	 */
	public static class SignedDistanceCharacters {
		
		protected static final int genFontSize = 35;
		protected static final int padding = 8;
		protected static final Img FONTMETRIC_IMG = cp.new Img(64, 64);
		
		/**
		 * Character set for instances of {@link SignedDistanceCharacters}.
		 * Characters in this string are sorted by char value ascending.
		 */
		public static final String CHARACTERS = 
				" !\"#$%&'()*+,-./0123456789:;<=>?@" +
				"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`" +
				"abcdefghijklmnopqrstuvwxyz{|}~";
				
	}

	/**
	 * The CharacterAtlas class is a texture atlas for looking up character textures.
	 * That is a texture containing rendered characters at specific positions.
	 * This is used to be able to render an arbitrary sequence of characters by looking up
	 * the corresponding area in the texture.
	 * A {@link CharacterAtlas} is defined by:
	 * <ul>
	 * <li>its font size</li>
	 * <li>its font style (PLAIN, ITALIC, BOLD, BOLD|ITALIC)</li>
	 * </ul>
	 * The font used is Ubuntu Mono which is a monospaced font, which can be accessed through
	 * the {@link FontProvider}.
	 * This implementation limits the possible characters to the ones listed in 
	 * {@link SignedDistanceCharacters#CHARACTERS}.
	 * Any other character will be mapped to white space and will thus be invisible in the
	 * render.
	 * To obtain a character atlas use the static {@link CharacterAtlas#get(int, int)}
	 * method.
	 * A {@link VertexArray} with 2D vertices on the first attribute and 2D texture coordinates
	 * on the second can be retrieved for a specified string using 
	 * {@link CharacterAtlas#createVAforString(String, VertexArray)}.
	 * 
	 * @author hageldave
	 */
	public static class CharacterAtlas {

		private static final char[] CHARACTERS = SignedDistanceCharacters.CHARACTERS.toCharArray();

		protected static final Img FONTMETRIC_IMG = cp.new Img(32, 32);

		protected static final HashMap<Integer, HashMap<GenericKey, CharacterAtlas>> ATLAS_COLLECTION = new HashMap<>();
		
		protected static final HashMap<Integer, HashMap<Integer, int[]>> CONTEXT_2_STYLE_2_TEXTUREREF = new HashMap<>();
		
		protected static final float leftPaddingFactor = 0.1f;
		
		protected static final float rightPaddingFactor = 0.3f;
		
		protected static final float topPaddingFactor = 0.1f;
		
		protected static final float botPaddingFactor = 0.1f;

		public Font font;
		public int charWidth;
		public int charHeigth;
		public int fontSize;
		public int style;
		public int owningCanvasID;
		protected int texID;
		public SignedDistanceCharacters sdChars;
		
		/**
		 * Calls {@link #boundsForText(int, Font)} with corresponding Ubuntu Mono font.
		 * @param textlength number of characters
		 * @param fontSize point size of the font
		 * @param style of the font e.g. {@link Font#PLAIN}.
		 * @return bounding rectangle for a text of specified length and font.
		 */
		public static Rectangle2D boundsForText(int textlength, int fontSize, int style){
			//Font font = FontProvider.getUbuntuMono(fontSize, style);
			Font font = MipavUtil.font12;
			return boundsForText(textlength, font);
		}
		
		/**
		 * Calculates the bounding rectangle for a specific number of characters in the specified font.
		 * The returned bounds are baseline relative which means that the origin may have a negative
		 * y coordinate that is the distance of the descent line from the baseline.
		 * @param textlength number of characters
		 * @param font to measure with (has to be monospaced, or else bounds will be incorrect)
		 * @return bounding rectangle for a text of specified length and font.
		 */
		public static Rectangle2D boundsForText(int textlength, Font font){
			Graphics2D g2d = FONTMETRIC_IMG.createGraphics();
			FontMetrics fontMetrics = g2d.getFontMetrics(font);
			char[] sampletext = new char[textlength]; Arrays.fill(sampletext, 'K');
			Rectangle2D bounds = fontMetrics.getStringBounds(new String(sampletext), g2d);
			g2d.dispose();
			return bounds;
		}


	}
	
	public static final class FontProvider {
		
		public static final String UBUNTU_MONO_PLAIN_RESOURCE = "/font/UbuntuMono-R.ttf";
		public static final String UBUNTU_MONO_BOLD_RESOURCE = "/font/UbuntuMono-B.ttf";
		public static final String UBUNTU_MONO_ITALIC_RESOURCE = "/font/UbuntuMono-RI.ttf";
		public static final String UBUNTU_MONO_BOLDITALIC_RESOURCE = "/font/UbuntuMono-BI.ttf";

		public static final Font UBUNTU_MONO_PLAIN = registerTrueTypeFont(
				FontProvider.class.getResource(UBUNTU_MONO_PLAIN_RESOURCE));
		
		public static final Font UBUNTU_MONO_BOLD = registerTrueTypeFont(
				FontProvider.class.getResource(UBUNTU_MONO_BOLD_RESOURCE));
		
		public static final Font UBUNTU_MONO_ITALIC = registerTrueTypeFont(
				FontProvider.class.getResource(UBUNTU_MONO_ITALIC_RESOURCE));
		
		public static final Font UBUNTU_MONO_BOLDITALIC = registerTrueTypeFont(
				FontProvider.class.getResource(UBUNTU_MONO_BOLDITALIC_RESOURCE));
		
		/**
		 * Loads a true type font (ttf) from the specified source
		 * and registers it with the local {@link GraphicsEnvironment}.
		 * 
		 * @param resource to load the ttf from
		 * @return the loaded and registered font
		 * @throws IllegalArgumentException when an IOException occurs
		 * or the specified resource is not in ttf format.
		 */
		public static Font registerTrueTypeFont(URL resource){
			try(InputStream is = resource.openStream()){
				Font font = Font.createFont(Font.TRUETYPE_FONT, is).deriveFont(12f);
				GraphicsEnvironment environment = GraphicsEnvironment.getLocalGraphicsEnvironment();
				environment.registerFont(font);
				return font;
			} catch (IOException | FontFormatException e) {
				throw new IllegalArgumentException(e);
			}
		}
		
		/**
		 * Returns the Ubuntu Mono font for the specified font size and style
		 * @param size font size 
		 * @param style font style, {@link Font#PLAIN}, {@link Font#BOLD}, {@link Font#ITALIC} 
		 * or BOLD|ITALIC
		 * @return font of specified size and style
		 */
		public static Font getUbuntuMono(float size, int style){
			switch (style) {
			case Font.PLAIN:
				return UBUNTU_MONO_PLAIN.deriveFont(size);
			case Font.BOLD:
				return UBUNTU_MONO_BOLD.deriveFont(size);
			case Font.ITALIC:
				return UBUNTU_MONO_ITALIC.deriveFont(size);
			case (Font.BOLD | Font.ITALIC):
				return UBUNTU_MONO_BOLDITALIC.deriveFont(size);
			default:
				throw new IllegalArgumentException(
						"Style argument is malformed. Only PLAIN, BOLD, ITALIC or BOLD|ITALIC are accepted.");
			}
		}
		
	}

	
	/**
	 * The GenericKey class can be used as key in {@link HashMap}s when
	 * multiple objects should be combined to a single key.
	 * The hash key is then generated using {@link Arrays#hashCode(Object[])}
	 * on the objects combined in this key.
	 * <br>
	 * This is for example used by the atlas collection in {@link CharacterAtlas}
	 * where the attributes fontSize, style and antialiasing are used to
	 * lookup the correct atlas.
	 * 
	 * @author hageldave
	 */
	public class GenericKey {

		private Object[] keycontents;
		
		/**
		 * Creates a new key from the specified objects
		 * @param objects the objects to form a key from (ordering changes hash)
		 */
		public GenericKey(Object ...objects) {
			this.keycontents = objects;
		}
		
		@Override
		public boolean equals(Object obj) {
			if(Objects.isNull(obj) || ! (obj instanceof GenericKey))
				return false;
			GenericKey other = (GenericKey) obj;
			return Arrays.equals(keycontents, other.keycontents);
		}
		
		@Override
		public int hashCode() {
			return Arrays.hashCode(keycontents);
		}
		
		/**
		 * @return the number of components this key consists of
		 */
		public int numComponents(){
			return keycontents.length;
		}
		
		/**
		 * Returns the component at specified index
		 * @param i index
		 * @return key component
		 */
		public Object getComponent(int i){
			return keycontents[i];
		}
		
		/**
		 * Returns the component at specified index
		 * casted to the specified type
		 * @param i index
		 * @param clazz type to cast to
		 * @return key component
		 * @param <T> type of the component
		 */
		public <T> T getComponent(int i, Class<T> clazz){
			return clazz.cast(keycontents[i]);
		}
		
	}

	/**
	 * The PointeredPoint2D class is an implementation of the {@link Point2D}
	 * class that uses two double arrays of size 1 to store its x and y coordinate.
	 * This allows for referencing the x or y coordinate elsewhere 
	 * (kind of as a pointer to the value) e.g. in another PointeredPoint2D
	 * so that 2 points can share a coordinate, for example points on the same axis.
	 * 
	 * @author hageldave
	 */
	public class PointeredPoint2D extends Point2D {
		
		/** 'pointer' to x coordinate */
		public final double[] x;
		/** 'pointer' to y coordinate */
		public final double[] y;
		
		/**
		 * Creates point at (0,0)
		 */
		public PointeredPoint2D() {
			x = new double[1];
			y = new double[1];
		}
		
		/**
		 * Creates point at (x,y)
		 * @param x coordinate
		 * @param y coordinate
		 */
		public PointeredPoint2D(double x, double y) {
			this();
			setLocation(x, y);
		}
		
		/**
		 * Creates point using the specified arrays as pointers for this point's coordinates
		 * @param xptr array to use as pointer to x coordinate
		 * @param yptr array to use as pointer to y coordinate
		 */
		public PointeredPoint2D(double[] xptr, double[] yptr){
			x = xptr;
			y = yptr;
		}

		@Override
		public double getX() {
			return x[0];
		}

		@Override
		public double getY() {
			return y[0];
		}

		@Override
		public void setLocation(double x, double y) {
			this.x[0] = x;
			this.y[0] = y;
		}
		
		@Override
		public Object clone() {
			return new PointeredPoint2D(getX(), getY());
		}
		
		@Override
		public String toString() {
			return String.format(Locale.US, "%s[%f, %f]", getClass().getSimpleName(), getX(),getY());
		}

	}


}