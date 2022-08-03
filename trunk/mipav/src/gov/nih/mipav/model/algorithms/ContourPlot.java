package gov.nih.mipav.model.algorithms;

import gov.nih.mipav.model.algorithms.ContourPlot.PixelConvertingSpliterator.PixelConverter;
import gov.nih.mipav.view.*;

import java.util.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowListener;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RectangularShape;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.awt.image.DataBuffer;
import java.awt.image.DataBufferInt;
import java.awt.image.DirectColorModel;
import java.awt.image.ImageObserver;
import java.awt.image.Raster;
import java.awt.image.RenderedImage;
import java.awt.image.WritableRaster;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.ref.WeakReference;
import java.net.URL;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.util.List;
import java.util.concurrent.CountedCompleter;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.DoubleBinaryOperator;
import java.util.function.DoubleSupplier;
import java.util.function.Function;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.imageio.ImageIO;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

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
	Example from source ContourPlot.java draws contour plot of function surface correctly.
	Need to work on example in IsolinesViz.java.
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
		coordsys.setCoordinateView(-1,-1,1,1);
		// set the content renderer of the coordinate system 
		// we want to render Lines and Triangles objects
		CompleteRenderer content = new CompleteRenderer();

		content.addItemToRender(contourbands).addItemToRender(contourlines);
		coordsys.setContent(content);
		
		// display within a JFrame
		JFrame frame = new JFrame();
		boolean useOpenGL = false;
		JPlotterCanvas canvas = null;
		if (useOpenGL) {
			//canvas = new BlankCanvas();
		}
		else {
			canvas = new BlankCanvasFallback();
		}
		canvas.setRenderer(coordsys);
		canvas.asComponent().setPreferredSize(new Dimension(400, 400));
		canvas.asComponent().setBackground(Color.WHITE);
		frame.getContentPane().add(canvas.asComponent());
		frame.setTitle("contourplot");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		canvas.addCleanupOnWindowClosingListener(frame);
		// make visible on AWT event dispatch thread
		SwingUtilities.invokeLater(()->{
			frame.pack();
			frame.setVisible(true);
		});

		/* we cheated a bit before when creating the contour bands,
		 * we assumed the colormap to be linear and just used the
		 * 3 colors we extracted. Since the color map is not linear
		 * we need to calculate bands for each step of the map so that
		 * its color scheme is preserved.
		 */
		double loIso=-0.5, hiIso=0.5;
		List<TriangleDetails> tris = new LinkedList<>();
		for(int i=0; i<colormap.numColors()-1; i++){
			double m1 = colormap.getLocation(i);
			double m2 = colormap.getLocation(i+1);
			// calc iso values corresponding to locations in color map
			double iso1 = loIso+m1*(hiIso-loIso);
			double iso2 = loIso+m2*(hiIso-loIso);
			int color1 = colormap.getColor(i);
			int color2 = colormap.getColor(i+1);
			tris.addAll(computeContourBands(X,Y,Z, iso1, iso2, color1, color2));
		}
		// replace triangles
		contourbands.removeAllTriangles();
		contourbands.getTriangleDetails().addAll(tris);
		canvas.scheduleRepaint();

		long t=System.currentTimeMillis()+2000;
		while(t>System.currentTimeMillis());
		if("true".equals("true")) {
			SwingUtilities.invokeLater(()->{
				Img img = new Img(frame.getSize());
				img.paint(g2d->frame.paintAll(g2d));
				ImageSaver.saveImage(img.getRemoteBufferedImage(), "C:/contour/howto_contourplot.png");
			});
			/*SwingUtilities.invokeLater(()->{
				try (PDDocument doc = canvas.paintPDF())
				{
					doc.save(new File("howto_contourplot.pdf"));
					doc.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
				System.out.println("exported pdf");
			});*/
		}


     	System.out.println("Finished runAlgorithm()");
	} // public void runAlgorithm()

	/**
	 * Class providing convenience methods for saving Images to file.
	 * @author hageldave
	 * @since 1.0
	 */
	public static class ImageSaver {
		
		private ImageSaver(){}
		
		/**
		 * @return {@link ImageIO#getWriterFileSuffixes()}
		 * @since 1.0
		 */
		public static String[] getSaveableImageFileFormats(){
			return ImageIO.getWriterFileSuffixes();
		}
		
		/**
		 * returns if specified image format supports rgb values only. This means
		 * argb values probably need conversion beforehand.
		 * @param imgFormat the file format e.g. jpg or png
		 * @return true if format only supports rgb values
		 * @since 1.0
		 */
		public static boolean isFormatRGBOnly(String imgFormat){
			switch (imgFormat.toLowerCase()) {
			/* fall through */
			case "jpg":
			case "jpeg":
			case "bmp":
				return true;
			default:
				return false;
			}
		}
		
		public static boolean isFormatBWOnly(String imgFormat){
			switch (imgFormat.toLowerCase()) {
			case "wbmp":
				return true;
			default:
				return false;
			}
		}
		
		/**
		 * Saves specified Image to file of specified img file format.
		 * <p>
		 * Some image file formats may require image type conversion before
		 * saving. This method converts to type INT_RGB for jpg, jpeg and bmp, 
		 * and to type BYTE_BINARY for wbmp.
		 * <p>
		 * The provided {@link OutputStream} will not be closed, this is the
		 * responsibility of the caller.
		 * @param image to be saved
		 * @param os {@link OutputStream} to write image to.
		 * @param imgFileFormat image file format. Consult {@link #getSaveableImageFileFormats()}
		 * to get the supported img file formats of your system. 
		 * @throws ImageSaverException if an IOException occurred during 
		 * the process or no appropriate writer could be found for specified format.
		 * @since 1.1
		 */
		public static void saveImage(Image image, OutputStream os, String imgFileFormat){
			final RenderedImage rImg;
			if( isFormatRGBOnly(imgFileFormat) && !isBufferedImageOfType(image, BufferedImage.TYPE_INT_RGB) ){
				rImg = BufferedImageFactory.get(image, BufferedImage.TYPE_INT_RGB);
			} else 
			if( isFormatBWOnly(imgFileFormat)  && !isBufferedImageOfType(image, BufferedImage.TYPE_BYTE_BINARY) ){
				rImg = BufferedImageFactory.get(image, BufferedImage.TYPE_BYTE_BINARY);
			} else
			if( image instanceof RenderedImage ){
				rImg = (RenderedImage) image;
			} else {
				rImg = BufferedImageFactory.getINT_ARGB(image);
			}
			try { 
				boolean success = ImageIO.write(rImg, imgFileFormat, os);
				if(!success){
					throw new ImageSaverException("Could not save Image! No appropriate writer was found.");
				}
			} catch (IOException e) {
				throw new ImageSaverException(e);
			}
		}
		
		private static final boolean isBufferedImageOfType(Image image, int type){
			return image instanceof BufferedImage && ((BufferedImage)image).getType() == type;
		}
		
		/**
		 * Saves image using {@link #saveImage(Image, File, String)}.
		 * @param image to be saved
		 * @param filename path to file
		 * @param imgFileFormat image file format. Consult {@link #getSaveableImageFileFormats()}
		 * @throws ImageSaverException if an IOException occurred during 
		 * the process, the provided filename path does refer to a directory or no 
		 * appropriate writer could be found for specified format.
		 * @since 1.0 
		 */
		public static void saveImage(Image image, String filename, String imgFileFormat){
			File f = new File(filename);
			if(!f.isDirectory()){
				saveImage(image, f, imgFileFormat);
			} else {
				throw new ImageSaverException(new IOException(String.format("provided file name denotes a directory. %s", filename)));
			}
		}
		
		/**
		 * Saves image using {@link #saveImage(Image, OutputStream, String)}.
		 * @param image to be saved
		 * @param file to save image to
		 * @param imgFileFormat image file format. Consult {@link #getSaveableImageFileFormats()}
		 * @throws ImageSaverException if an IOException occurred during 
		 * the process, no OutputStream could be created due to a 
		 * FileNotFoundException or no appropriate writer could be found for 
		 * specified format.
		 * @since 1.0
		 */
		public static void saveImage(Image image, File file, String imgFileFormat){
			try (FileOutputStream fos = new FileOutputStream(file)){
				saveImage(image, fos, imgFileFormat);
			} catch (IOException e) {
				throw new ImageSaverException(e);
			}
		}
		
		/**
		 * Saves Image using {@link #saveImage(Image, File, String)}. The file
		 * format is extracted from the files name.
		 * @param image to be saved
		 * @param file to save image to
		 * @throws ImageSaverException if an IOException occurred during 
		 * the process, the filename does not contain a dot to get the filetype
		 * or no appropriate writer could be found for specified format.
		 * @since 1.0
		 */
		public static void saveImage(Image image, File file){
			// get file ending
			int dotIndex = file.getName().lastIndexOf('.');
			if(dotIndex >= 0){
				String ending = file.getName().substring(dotIndex+1, file.getName().length());
				saveImage(image, file, ending);
			} else {
				throw new ImageSaverException("could not detect file format from file name. Missing dot. " + file.getName());
			}
		}
		
		/**
		 * Saves Image using {@link #saveImage(Image, File)}. The file
		 * format is extracted from the files name.
		 * @param image to be saved
		 * @param filename path to file to save image to
		 * @throws ImageSaverException if an IOException occurred during 
		 * the process, the filename does not contain a dot to get the filetype,
		 * the provided filename path does refer to a directory, 
		 * or no appropriate writer could be found for specified format.
		 * @since 1.0
		 */
		public static void saveImage(Image image, String filename){
			File f = new File(filename);
			if(!f.isDirectory()){
				saveImage(image, f);
			} else {
				throw new ImageSaverException(new IOException(String.format("provided file name denotes a directory. %s", filename)));
			}
		}
		
		/**
		 * RuntimeException class for Exceptions that occur during image saving.
		 * @author hageldave
		 * @since 1.1
		 */
		public static class ImageSaverException extends RuntimeException {
			private static final long serialVersionUID = -2590926614530103717L;

			public ImageSaverException() {
			}
			
			public ImageSaverException(String message) {
				super(message);
			}
			
			public ImageSaverException(Throwable cause) {
				super(cause);
			}
			
			public ImageSaverException(String message, Throwable cause) {
				super(message, cause);
			}
		}
	}

	
	/**
	 * This interface defines the methods required by an implementation of a 
	 * canvas {@link Component} for use with JPlotter {@link Renderer}s such as 
	 * {@link BlankCanvas} or {@link BlankCanvasFallback}.
	 * 
	 * The most important required operations on such a component are:
	 * <ul>
	 * <li>setting a {@link Renderer} that takes care of generating the displayed content</li>
	 * <li>exporting what the component displays to an image</li>
	 * <li>querying a pixel color at a specific location of the component (especially important for picking)</li>
	 * <li>scheduling a repaint operation of the component</li>
	 * <li>getting this as an awt {@link Component} (implicit cast) since implementations have to be an instance of this class</li>
	 * </ul>
	 * <p>
	 * This interface is intended to enable development without explicitly choosing between 
	 * {@link BlankCanvas} or {@link BlankCanvasFallback}.
	 * This way a fallback mode for an application can be easily realized, e.g. for macOS which is not supported by
	 * lwjgl3-awt and thus cannot use the OpenGL backed BlankCanvas.
	 * <p>
	 * JPlotterCanvas also provides the ability to export to scalable vector graphics (SVG) 
	 * and portable document format (PDF) with the {@link #paintSVG()} and {@link #paintPDF()} methods.
	 * 
	 * 
	 * @author hageldave
	 */
	public interface JPlotterCanvas {

		/**
		 * On AWT event dispatch thread:<br>
		 * Uses the set {@link Renderer} render to render display contents, then calls super.repaint() to display rendered content.
		 * <p>
		 * Schedules a repaint call call on the AWT event dispatch thread if not on it.
		 * <p>
		 * <b>This method is only deprecated for calling directly, call {@link #scheduleRepaint()} instead.</b><br>
		 * Of course super.repaint() is implemented by the implementing {@link Component} already.
		 */
		@Deprecated
		public void repaint();
		
		/**
		 * Schedules a repaint call on the AWT event dispatch thread.
		 * If a repaint is already pending, this method will not schedule an
		 * additional call until the render method within repaint is about to be executed.
		 */
		public void scheduleRepaint();
		
		/**
		 * En/disables SVG rendering as image.
		 * When rendering to SVG and this is enabled, instead of translating the 
		 * contents of the renderers into SVG elements, the current framebuffer image 
		 * is used and put into the dom.
		 * <p>
		 * This can be useful for example when too many SVG elements would be created
		 * resulting in a huge dom and file size when exporting as SVG.
		 * 
		 * @param enable true when no SVG elements should be created from the content
		 * of this JPlotterCanvas but instead a simple image element with the framebuffer's
		 * content.
		 */
		public void enableSvgAsImageRendering(boolean enable);
		
		/**
		 * @return true when enabled
		 * @see #enableSvgAsImageRendering(boolean)
		 */
		public boolean isSvgAsImageRenderingEnabled();

		/**
		 * En/disables PDF rendering as image.
		 * When rendering to PDF and this is enabled, instead of translating the
		 * contents of the renderers into PDF elements, the current framebuffer image
		 * is used and put into the pdf document.
		 * <p>
		 * This can be useful for example when too many PDF elements would be created
		 * resulting in a huge dom and file size when exporting as PDF.
		 *
		 * @param enable true when no PDF elements should be created from the content
		 * of this JPlotterCanvas but instead a simple image element with the framebuffer's
		 * content.
		 */
		public void enablePDFAsImageRendering(boolean enable);

		/**
		 * @return true when enabled
		 * @see #enablePDFAsImageRendering(boolean) (boolean)
		 */
		public boolean isPDFAsImageRenderingEnabled();


		/**
		 * Fetches the current contents of the framebuffer and returns them as an {@link Img}.
		 * @return image of the current framebuffer.
		 */
		public Img toImg();
		
		/**
		 * Reads the color value of the pixel at the specified location if areaSize == 1.
		 * This can be used to get the color or picking color under the mouse cursor.
		 * <p>
		 * Since the cursor placement may be inexact and thus miss the location the user
		 * was actually interested in, the areaSize parameter can be increased to create
		 * a window of pixels around the specified location.
		 * This window area will be examined and the most prominent non zero color value will
		 * be returned.
		 * @param x coordinate of the pixels location
		 * @param y coordinate of the pixels location
		 * @param picking whether the picking color or the visible color should be retrieved.
		 * @param areaSize width and height of the area around the specified location.
		 * @return the most prominent color in the area as integer packed ARGB value.
		 * If the returned value is to be used as an object id from picking color, then the
		 * alpha value probably has to be discarded first using {@code 0x00ffffff & returnValue}.
		 */
		public int getPixel(int x, int y, boolean picking, int areaSize);
		
		/**
		 * Creates a new SVG {@link Document} and renders this canvas as SVG elements.
		 * Will call {@link #paintToSVG(Document, Element, int, int)} after setting up
		 * the document and creating the initial elements.
		 * @return the created document
		 */
		/*public default Document paintSVG(){
			Document document = SVGUtils.createSVGDocument(asComponent().getWidth(), asComponent().getHeight());
			paintSVG(document, document.getDocumentElement());
			return document;
		}*/
		
		/**
		 * Renders this canvas as SVG elements under the specified parent element.
		 * Will call {@link #paintToSVG(Document, Element, int, int)} after creating 
		 * the initial elements.
		 * @param document document to create SVG elements with
		 * @param parent the parent node to which this canvas is supposed to be rendered
		 * to.
		 */
		/*public default void paintSVG(Document document, Element parent) {
			int w,h;
			if((w=asComponent().getWidth()) >0 && (h=asComponent().getHeight()) >0){
				if(SVGUtils.getDefs(document) == null){
					Element defs = SVGUtils.createSVGElement(document, "defs");
					defs.setAttributeNS(null, "id", "JPlotterDefs");
					document.getDocumentElement().appendChild(defs);
				}
				
				Element rootGroup = SVGUtils.createSVGElement(document, "g");
				parent.appendChild(rootGroup);
				rootGroup.setAttributeNS(null, "transform", "scale(1,-1) translate(0,-"+h+")");
				
				Element background = SVGUtils.createSVGElement(document, "rect");
				rootGroup.appendChild(background);
				background.setAttributeNS(null, "id", "background"+"@"+hashCode());
				background.setAttributeNS(null, "width", ""+w);
				background.setAttributeNS(null, "height", ""+h);
				background.setAttributeNS(null, "fill", SVGUtils.svgRGBhex(asComponent().getBackground().getRGB()));
				
				paintToSVG(document, rootGroup, w,h);
			}
		}*/
		
		/**
		 * Renders this {@link JPlotterCanvas} in terms of SVG elements
		 * to the specified parent element of the specified SVG document.
		 * <p>
		 * This method has to be overridden when the implementing
		 * class can express its contents in terms of SVG.
		 * 
		 * @param doc document to create svg elements with
		 * @param parent to append svg elements to
		 * @param w width of the viewport (the width of this Canvas)
		 * @param h height of the viewport (the height of this Canvas)
		 */
		/*public default void paintToSVG(Document doc, Element parent, int w, int h){
			Renderer renderer = getRenderer();
			if(renderer != null) 
				renderer.renderSVG(doc, parent, w, h);
		}*/

		/**
		 * Creates a new PDF document {@link PDDocument} and renders this canvas on the PDF document.
		 * Will call {@link #paintPDF()} after creating
		 * the document and the first page.
		 *
		 * @return the resulting pdf document with all the rendered content
		 * @throws IOException If there is an error while creating the document.
		 */
		/*public default PDDocument paintPDF() throws IOException {
			PDDocument document = new FontCachedPDDocument();
			PDPage page = new PDPage();
			document.addPage(page);
			paintPDF(document, page);
			return document;
		}*/

		/**
		 * Sets up the PDF document (size, background color, ...).
		 * Will call {@link #paintToPDF(PDDocument, PDPage, int, int)} after setting up
		 * the document and creating the initial elements.
		 *
		 * @param document PDF document holding the page
		 * @param page Page in the document to create PDF elements in
		 * @throws IOException If there is an error while creating the document.
		 */
		/*public default void paintPDF(PDDocument document, PDPage page) throws IOException {
			int w,h;
			if ((w=asComponent().getWidth()) > 0 && (h=asComponent().getHeight()) > 0) {
				page.setMediaBox(new PDRectangle(w, h));
				PDPageContentStream contentStream = new PDPageContentStream(document, page,
						PDPageContentStream.AppendMode.APPEND, false);
				contentStream.addRect(0, 0, w, h);
				contentStream.setNonStrokingColor(asComponent().getBackground());
				contentStream.fill();
				contentStream.close();
				paintToPDF(document, page, w, h);
			}
		}*/


		/*public default void paintPDF(PDDocument document, PDPage page, PDPageContentStream contentStream, Rectangle2D renderLoc) throws IOException {
			contentStream.addRect(0, 0, page.getMediaBox().getWidth(), page.getMediaBox().getHeight());
			contentStream.setNonStrokingColor(asComponent().getBackground());
			contentStream.fill();
			paintToPDF(document, page, renderLoc);
		}*/


		/*public default void paintToPDF(PDDocument document, PDPage page, Rectangle2D renderLoc) {
			Renderer renderer = getRenderer();
			if(renderer != null) {
				renderer.renderPDF(document, page,
						(int) renderLoc.getX(),
						(int) (page.getMediaBox().getHeight()-renderLoc.getMaxY()),
						(int) renderLoc.getMaxX(),
						(int) (page.getMediaBox().getHeight()-renderLoc.getY()));
			}
		}*/

		/**
		 * Renders this {@link JPlotterCanvas} in terms of PDF elements
		 * to the specified page of the specified PDF document.
		 *
		 * @param document PDF document holding the page
		 * @param page page in the document to create PDF elements in
		 * @param w width of the page
		 * @param h height of the page
		 */
		public default void paintToPDF(PDDocument document, PDPage page, int w, int h) {
			Renderer renderer = getRenderer();
			if(renderer != null)
				renderer.renderPDF(document, page, 0, 0, w, h);
		}

		/**
		 * Sets the renderer of this canvas.
		 * @param renderer to draw contents.
		 * @return this for chaining
		 */
		public JPlotterCanvas setRenderer(Renderer renderer);
		
		/**
		 * @return the current renderer
		 */
		public Renderer getRenderer();
		
		/**
		 * Implicit cast of this canvas to a class extending {@link Component}.
		 * This implies that the implementing class is a {@link Component}.
		 * @return this, but cast to {@link Component}
		 */
		public default Component asComponent() {
			return (Component)this;
		}
		
		/**
		 * Determines the most prominent value in a square shaped area.
		 * This method should be used by {@link #getPixel(int, int, boolean, int)} implementations.
		 * @param colors of the square shaped area
		 * @param areaSize widht or height of the area
		 * @return mode of colors, with +1 count for the center color
		 */
		public static int mostProminentColor(int[] colors, int areaSize) {
			if(areaSize == 1){
				return colors[0];
			}
			int center = areaSize*(areaSize/2)+(areaSize/2);
			int centerValue = colors[center];
			int centerBonus = centerValue == 0 ? 0:1;
			// calculate most prominent color (mode)
			Arrays.sort(colors);
			int currentValue = colors[0]; 
			int mostValue = currentValue; 
			int count = currentValue == centerValue ? 1+centerBonus:1; // center color gets bonus
			int maxCount=count;
			for(int i = 1; i < colors.length; i++){
				if(colors[i]==currentValue && currentValue != 0xff000000){
					count++;
				} else {
					if(count > maxCount){
						maxCount = count;
						mostValue = currentValue;
					}
					currentValue = colors[i];
					count = currentValue == centerValue ? 1+centerBonus:1; // center color gets bonus
				}
			}
			return mostValue;
		}
		
		/**
		 * Adds a {@link WindowListener} to the specified window that takes care of
		 * cleanup (GL resources) when the window is about to close. 
		 * <p>
		 * This method only has an effect when this {@link JPlotterCanvas} is an instance of {@link FBOCanvas}
		 * which uses GL resources (see {@link FBOCanvas#createCleanupOnWindowClosingListener()}). 
		 * Otherwise no WindowListener is created or added.
		 * 
		 * @param window the window to add the listener to (should be the window containing this canvas)
		 * @return the added listener when this is an FBOCanvas, else null
		 */
		public default WindowListener addCleanupOnWindowClosingListener(Window window) {
			/*if(this instanceof FBOCanvas) {
				WindowListener l = ((FBOCanvas)this).createCleanupOnWindowClosingListener();
				window.addWindowListener(l);
				return l;
			}*/
			return null;
		}
		
	}

	
	/**
	 * Fallback implementation for {@link BlankCanvas} for systems that do not support OpenGL 3
	 * through {@link org.lwjgl.opengl.awt.AWTGLCanvas} (e.g. macOS).
	 * <p>
	 * This {@link JComponent} uses a single {@link Renderer} to draw its contents.
	 * 
	 * @author hageldave
	 */
	public class BlankCanvasFallback extends JComponent implements JPlotterCanvas {
		private static final long serialVersionUID = 1L;
		private final ImageObserver obs_allbits = Utils.imageObserver(ImageObserver.ALLBITS);

		protected AtomicBoolean repaintIsSheduled = new AtomicBoolean(false);
		protected Img mainRenderBuffer = new Img(0,0);
		protected Img pickingRenderBuffer = new Img(0,0);
		protected Img displayBuffer = new Img(0,0);
		protected Renderer renderer;
		protected boolean isRenderSvgAsImage = false;
		protected boolean isRenderPDFAsImage = false;
		
		/**
		 * Creates a new {@link BlankCanvasFallback} instance.
		 */
		public BlankCanvasFallback() {
			this.addComponentListener(new ComponentAdapter() {
				@Override
				public void componentResized(ComponentEvent e) {
					repaint();
				}
			});
		}

		@Override
		public BlankCanvasFallback setRenderer(Renderer renderer) {
			this.renderer = renderer;
			return this;
		}

		@Override
		public Renderer getRenderer() {
			return renderer;
		}

		@Override
		public void scheduleRepaint() {
			if(repaintIsSheduled.compareAndSet(false, true)){
				SwingUtilities.invokeLater(this::repaint);
			}
		}

		@Override
		public void repaint() {
			if(SwingUtilities.isEventDispatchThread()){
				repaintIsSheduled.set(false);
				render();
				super.repaint();
			} else {
				scheduleRepaint();
			}
		}

		protected void render() {
			int w=getWidth(); int h=getHeight();
			if(mainRenderBuffer.getWidth()!=w || mainRenderBuffer.getHeight()!=h) {
				mainRenderBuffer = new Img(w, h);
				pickingRenderBuffer = new Img(w, h);
			}
			if(w==0 && h==0)
				return;
			// clear / fill with clear color
			mainRenderBuffer.fill(getBackground().getRGB());
			pickingRenderBuffer.fill(0x00000000);
			// setup render graphics
			Graphics2D g=null,p=null;
			try {
				g=mainRenderBuffer.createGraphics();
				
				p=pickingRenderBuffer.createGraphics();
				g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE);
				g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP);
				
				p.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
				p.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
				p.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
				
				g.translate(0, h);
				g.scale(1.0, -1.0);
				p.translate(0, h);
				p.scale(1.0, -1.0);
				render(g,p, w,h);
			} finally {
				if(g!=null)g.dispose();
				if(p!=null)p.dispose();
			}
		}

		protected void render(Graphics2D g, Graphics2D p, int w, int h) {
			if(renderer != null) {
				renderer.renderFallback(g, p, w, h);
			}
		}
		
		

		@Override
		public void paint(Graphics g) {
			// test if this is SVG painting
			/*if(g instanceof SVGGraphics2D && !isSvgAsImageRenderingEnabled()){
				return;
			}
			if (g instanceof PdfBoxGraphics2D && !isPDFAsImageRenderingEnabled()) {
				return;
			}*/
			
			g.clearRect(0, 0, getWidth(), getHeight());
			int w=mainRenderBuffer.getWidth();
			int h=mainRenderBuffer.getHeight();
			if(w>0&&h>0) {
				g.drawImage(mainRenderBuffer.getRemoteBufferedImage(), 
						0, 0, getWidth(), getHeight(), 
						0, 0, w, h, 
						obs_allbits);
			}
		}

		@Override
		public void enableSvgAsImageRendering(boolean enable){
			this.isRenderSvgAsImage = enable;
		}
		
		@Override
		public boolean isSvgAsImageRenderingEnabled(){
			return isRenderSvgAsImage;
		}

		@Override
		public void enablePDFAsImageRendering(boolean enable) {
			this.isRenderPDFAsImage = enable;
		}

		@Override
		public boolean isPDFAsImageRenderingEnabled() {
			return isRenderPDFAsImage;
		}

		@Override
		public Img toImg() {
			return mainRenderBuffer.copy();
		}

		@Override
		public int getPixel(int x, int y, boolean picking, int areaSize) {
			Img img = picking ? pickingRenderBuffer:mainRenderBuffer;
			Img area = new Img(areaSize, areaSize);
			area.forEach(px->{
				int v = img.getValue(x+px.getX()-areaSize/2, y+px.getY()-areaSize/2, 0);
				px.setValue(v);
			});
			int[] colors = area.getData();
			return JPlotterCanvas.mostProminentColor(colors, areaSize);
		}
		
		@Override
		public BlankCanvasFallback asComponent() {
			return this;
		}

	}

	
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
		 * Changes the saturation of a color.
		 * @param argb int packed argb color value
		 * @param s saturation multilpier (1 is no change, 0 is greyscale)
		 * @return saturation changed color
		 */
		public static int changeSaturation(int argb, double s) {
			double tounit = 1/255.0;
			double r=r(argb)*tounit, g=g(argb)*tounit, b=b(argb)*tounit;
			double l = r*0.2126 + g*0.7152 + b*0.0722; // luminance
			double dr=r-l, dg=g-l, db=b-l;
			if(s > 1.0) {
				// find maximal saturation that will keep channel values in range [0,1]
				//s*dr+l=1 -> s*dr=1-l -> (1-l)/dr=s
				//s*dr+l=0 -> s*dr=-l  -> (0-l)/dr=s
				s = Math.min(s, dr<0 ? -l/dr:(1-l)/dr);
				s = Math.min(s, dg<0 ? -l/dg:(1-l)/dg);
				s = Math.min(s, db<0 ? -l/db:(1-l)/db);
			}
			r=l+s*dr; g=l+s*dg; b=l+s*db;
			return (0xff000000 & argb) | (0x00ffffff & rgb_fromNormalized(r, g, b)); // bit operations are for alpha preservation
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
		 * Scales the alpha value of a specified integer packed ARGB color by a
		 * specified scaling factor {@code m}. 
		 * New color will be {@code (a*m, r, g, b)}.
		 * @param color of which alpha will be scaled
		 * @param m scaling factor
		 * @return integer packed ARGB color with scaled alpha
		 */
		public static int scaleColorAlpha(int color, double m) {
			double af = a_normalized(color)*m;
			int a = argb_fromNormalized(af, 0,0,0);
			return (color&0x00ffffff)|a;
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
			 * Returns the maximum of 3 values
			 * @param v0 value
			 * @param v1 value
			 * @param v2 value
			 * @return maximum
			 */
			public static double max3(double v0, double v1, double v2){
				return Math.max(Math.max(v0, v1), v2);
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

			/**
			 * Creates an {@link ImageObserver} that checks the for the specified infoflags.
			 * See {@link ImageObserver#imageUpdate(java.awt.Image, int, int, int, int, int)}.
			 * @param flags infoflags to check 
			 * @return a new ImageObserver
			 */
			public static ImageObserver imageObserver(int flags) {
				return (image, infoflags, x, y, width, height)->(infoflags & flags)!=flags;
			}

			/**
			 * Calculates sqrt(x*x + y*y) which is the hypotenuse length given catheti x and y.
			 * @param x a value
			 * @param y a value
			 * @return hypotenuse length
			 */
			public static final double hypot(double x, double y){
				return Math.sqrt(x*x + y*y);
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
	public class CompleteRenderer implements Renderer, AdaptableView {
		
		public final LinesRenderer lines = new LinesRenderer();
		//public final PointsRenderer points = new PointsRenderer();
		public final TextRenderer text = new TextRenderer();
		public final TrianglesRenderer triangles = new TrianglesRenderer();
		//public final CurvesRenderer curves = new CurvesRenderer();
		
		private final Renderer[] rendererLUT = new Renderer[]{triangles,lines,/*curves,points,*/text};
		//public static final int TRI = 0, LIN = 1, PNT = 2, TXT = 3, CRV = 4;
		public static final int TRI = 0, LIN = 1, TXT = 2;
		private final int[] renderOrder = {TRI,LIN,/*CRV,PNT,*/TXT};
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
			rendererLUT[renderOrder[2]].renderFallback(g, p, w, h);
			//rendererLUT[renderOrder[3]].renderFallback(g, p, w, h);
			//rendererLUT[renderOrder[4]].renderFallback(g, p, w, h);
		}
		
		public void renderPDF(PDDocument doc, PDPage page, int x, int y, int w, int h) {
			if(!isEnabled()){
				return;
			}
			rendererLUT[renderOrder[0]].renderPDF(doc, page, x, y, w, h);
			rendererLUT[renderOrder[1]].renderPDF(doc, page, x, y, w, h);
			rendererLUT[renderOrder[2]].renderPDF(doc, page, x, y, w, h);
			//rendererLUT[renderOrder[3]].renderPDF(doc, page, x, y, w, h);
			//rendererLUT[renderOrder[4]].renderPDF(doc, page, x, y, w, h);
		}
		
		/**
		 * Adds the specified item to the corresponding renderer.
		 * Only instances of {@link Triangles}, {@link Lines}, {@link Points} and {@link Text}
		 * are accepted, other item types result in an {@link IllegalArgumentException}. 
		 * @param item to add
		 * @return this for chaining
		 * @throws IllegalArgumentException when unsupported type of item is specified.
		 */
		public CompleteRenderer addItemToRender(Renderable item){
			if(item instanceof Triangles){
				triangles.addItemToRender((Triangles) item);
			} else 
			if(item instanceof Lines){
				lines.addItemToRender((Lines) item);
			//} else 
			//if(item instanceof Points){
				//points.addItemToRender((Points) item);
			} else
			if(item instanceof Text){
				text.addItemToRender((Text) item);
			//} else
			//if(item instanceof Curves){
				//curves.addItemToRender((Curves) item);
			} else {
				throw new IllegalArgumentException(
						"Cannot add Renderable of type " 
						+ item.getClass().getSimpleName()
						+ ". This type is not supported by this renderer."
				);
			}
			return this;
		}
		
		/**
		 * Sets the view matrix for each of the renderers
		 */
		@Override
		public void setView(Rectangle2D rect) {
			triangles.setView(rect);
			lines.setView(rect);
			//points.setView(rect);
			text.setView(rect);
			//curves.setView(rect);
		}


	}
	
	/**
	 * The TrianglesRenderer is an implementation of the {@link GenericRenderer}
	 * for {@link Triangles}.
	 * <br>
	 * Its fragment shader draws the picking color into the second render buffer
	 * alongside the 'visible' color that is drawn into the first render buffer.
	 * 
	 * @author hageldave
	 */
	public class TrianglesRenderer extends GenericRenderer<Triangles> {
		@Override
		public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
			if(!isEnabled()){
				return;
			}
			
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
			
			double translateX = Objects.isNull(view) ? 0:view.getX();
			double translateY = Objects.isNull(view) ? 0:view.getY();
			double scaleX = Objects.isNull(view) ? 1:w/view.getWidth();
			double scaleY = Objects.isNull(view) ? 1:h/view.getHeight();
			
			Rectangle2D viewportRect = new Rectangle2D.Double(0, 0, w, h);
			
			float[][] tricoords = new float[2][3];
			for(Triangles tris : getItemsToRender()){
				if(tris.isHidden()){
					continue;
				}
				if(tris.isAAinFallbackEnabled()) g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
				else g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
				for(TriangleDetails tri : tris.getIntersectingTriangles(view != null ? view:viewportRect)){
					
					double x0,y0, x1,y1, x2,y2;
					x0=tri.p0.getX(); y0=tri.p0.getY(); x1=tri.p1.getX(); y1=tri.p1.getY(); x2=tri.p2.getX(); y2=tri.p2.getY();
					
					x0-=translateX; x1-=translateX; x2-=translateX;
					y0-=translateY; y1-=translateY; y2-=translateY;
					x0*=scaleX; x1*=scaleX; x2*=scaleX;
					y0*=scaleY; y1*=scaleY; y2*=scaleY;
					
					tricoords[0][0]=(float)x0; tricoords[0][1]=(float)x1; tricoords[0][2]=(float)x2;
					tricoords[1][0]=(float)y0; tricoords[1][1]=(float)y1; tricoords[1][2]=(float)y2;

					int c0 = ColorOperations.changeSaturation(tri.c0.getAsInt(), tris.getGlobalSaturationMultiplier());
					c0 = ColorOperations.scaleColorAlpha(c0, tris.getGlobalAlphaMultiplier());
					int c1 = ColorOperations.changeSaturation(tri.c1.getAsInt(), tris.getGlobalSaturationMultiplier());
					c1 = ColorOperations.scaleColorAlpha(c1, tris.getGlobalAlphaMultiplier());
					int c2 = ColorOperations.changeSaturation(tri.c2.getAsInt(), tris.getGlobalSaturationMultiplier());
					c2 = ColorOperations.scaleColorAlpha(c2, tris.getGlobalAlphaMultiplier());
					
					g.setPaint(new BarycentricGradientPaint(tricoords[0], tricoords[1], new Color(c0, true), new Color(c1, true), new Color(c2, true)));
					

					int minx = (int)Utils.min3(x0, x1, x2);
					int miny = (int)Utils.min3(y0, y1, y2);
					double maxx = Utils.max3(x0, x1, x2);
					double maxy = Utils.max3(y0, y1, y2);
					g.fillRect((minx), (miny), (int)Math.ceil(maxx-minx), (int)Math.ceil(maxy-miny));
					if(tri.pickColor != 0) {
						Color pick=new Color(tri.pickColor);
						p.setPaint(new BarycentricGradientPaint(tricoords[0], tricoords[1], pick, pick, pick));
						p.fillRect((minx), (miny), (int)Math.ceil(maxx-minx), (int)Math.ceil(maxy-miny));
					}
				}
			}
			
			
			g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
		}
		
		
	
	}
	
	/**
	 * The BarycentricGradientPaint class provides a way to fill a {@link Shape}
	 * with a triangular color gradient. 
	 * Colors are specified for the vertices of a triangle and interpolated within
	 * the triangle according to barycentric coordinates.
	 * Only areas of the filled shape that are intersecting with the triangle of
	 * this paint are colored.
	 * <p>
	 * This paint supports the {@code ANTIALIASING} rendering hint 
	 * ({@link RenderingHints}) using a 4x multisampling approach. 
	 * When enabled, the edges of the triangle will appear anti-aliased.
	 * It is recommended to disable AA when filling a triangle mesh (where triangles
	 * are adjacent), since otherwise triangle edges become visible.
	 * <p>
	 * Note that it is not necessary to use a triangular {@link Shape} to render a 
	 * triangle. Instead a rectangle can be used as well, since only the intersecting 
	 * area will be filled.
	 * 
	 * @author hageldave
	 */
	public class BarycentricGradientPaint implements Paint {

		protected Point2D.Float p1;
		protected Point2D.Float p2;
		protected Point2D.Float p3;
		protected Color color1;
		protected Color color2;
		protected Color color3;


		/**
		 * Creates a new {@link BarycentricGradientPaint} object with
		 * specified triangle vertices and vertex colors.
		 * 
		 * @param p1 vertex of triangle
		 * @param p2 vertex of triangle
		 * @param p3 vertex of triangle
		 * @param color1 color of vertex
		 * @param color2 color of vertex
		 * @param color3 color of vertex
		 */
		public BarycentricGradientPaint(Point2D p1, Point2D p2, Point2D p3, Color color1, Color color2, Color color3) {
			this.p1 = new Point2D.Float((float)p1.getX(), (float)p1.getY());
			this.p2 = new Point2D.Float((float)p2.getX(), (float)p2.getY());
			this.p3 = new Point2D.Float((float)p3.getX(), (float)p3.getY());
			this.color1 = color1;
			this.color2 = color2;
			this.color3 = color3;
		}

		/**
		 * Creates a new {@link BarycentricGradientPaint} object with
		 * specified triangle vertices and vertex colors.
		 * 
		 * @param x x-coordinates for the triangle vertices
		 * @param y y-coordinates for the triangle vertices
		 * @param color1 color of vertex
		 * @param color2 color of vertex
		 * @param color3 color of vertex
		 */
		public BarycentricGradientPaint(float[] x, float[] y, Color color1, Color color2, Color color3) {
			this(x[0],y[0],x[1],y[1],x[2],y[2], color1,color2,color3);
		}

		/**
		 * Creates a new {@link BarycentricGradientPaint} object with
		 * specified triangle vertices and vertex colors.
		 * 
		 * @param x1 x-coord of triangle vertex
		 * @param y1 y-coord of triangle vertex
		 * @param x2 x-coord of triangle vertex
		 * @param y2 y-coord of triangle vertex
		 * @param x3 x-coord of triangle vertex
		 * @param y3 y-coord of triangle vertex
		 * @param color1 color of vertex
		 * @param color2 color of vertex
		 * @param color3 color of vertex
		 */
		public BarycentricGradientPaint(double x1, double y1, double x2, double y2, double x3, double y3, Color color1, Color color2, Color color3) {
			this((float)x1,(float)y1,(float)x2,(float)y2,(float)x3,(float)y3, color1,color2,color3);
		}

		/**
		 * Creates a new {@link BarycentricGradientPaint} object with
		 * specified triangle vertices and vertex colors.
		 * 
		 * @param x1 x-coord of triangle vertex
		 * @param y1 y-coord of triangle vertex
		 * @param x2 x-coord of triangle vertex
		 * @param y2 y-coord of triangle vertex
		 * @param x3 x-coord of triangle vertex
		 * @param y3 y-coord of triangle vertex
		 * @param color1 color of vertex
		 * @param color2 color of vertex
		 * @param color3 color of vertex
		 */
		public BarycentricGradientPaint(float x1, float y1, float x2, float y2, float x3, float y3, Color color1, Color color2, Color color3) {
			this.p1 = new Point2D.Float(x1, y1);
			this.p2 = new Point2D.Float(x2, y2);
			this.p3 = new Point2D.Float(x3, y3);
			this.color1 = color1;
			this.color2 = color2;
			this.color3 = color3;
		}
		
		@Override
		public int getTransparency() {
			int a1 = color1.getAlpha();
			int a2 = color2.getAlpha();
			int a3 = color3.getAlpha();
			return (((a1 & a2 & a3) == 0xff) ? OPAQUE : TRANSLUCENT);
		}
		
		@Override
		public PaintContext createContext(ColorModel cm, Rectangle deviceBounds, Rectangle2D userBounds,
				AffineTransform xform, RenderingHints hints) {
			return new BarycentricGradientPaintContext(
					p1,p2,p3, 
					color1,color2,color3, 
					xform, 
					hints.get(RenderingHints.KEY_ANTIALIASING) == RenderingHints.VALUE_ANTIALIAS_ON
			);
		}


	}

	/**
	 * This class is implements the {@link PaintContext} for 
	 * {@link BarycentricGradientPaint}.
	 * <p>
	 * The context operates solely in ARGB color space (blue on least 
	 * significant bits) with an integer packing {@link DirectColorModel}.
	 * <p>
	 * A cache for raster memory is implemented to avoid costly memory 
	 * allocations.
	 * 
	 * @author hageldave
	 */
	public class BarycentricGradientPaintContext implements PaintContext {
		protected final float[] MSAA_SAMPLES;

		{
			MSAA_SAMPLES = new float[8];
			AffineTransform xform = new AffineTransform();
			xform.translate(.5, .5);
			xform.rotate(Math.PI*0.5*0.2);
			xform.scale(.5, .5);
			xform.translate(-.5, -.5);
			xform.transform(new float[] {0,0, 1,0, 0,1, 1,1}, 0, MSAA_SAMPLES, 0, 4);
		}

		protected final float x1,x2,x3,y1,y2,y3;
		protected final float x23,x13,y23,y13; //,x12,y12;
		protected final float denom;

		protected final int c1,c2,c3;
		protected final DirectColorModel cm = new DirectColorModel(32,
				0x00ff0000,       // Red
				0x0000ff00,       // Green
				0x000000ff,       // Blue
				0xff000000        // Alpha
				);
		protected final boolean antialiasing;
		protected WritableRaster saved;
		protected WeakReference<int[]> cache;

		public BarycentricGradientPaintContext(
				Point2D.Float p1, Point2D.Float p2, Point2D.Float p3,
				Color color1, Color color2, Color color3,
				AffineTransform xform, boolean antialiasing) 
		{
			c1 = color1.getRGB();
			c2 = color2.getRGB();
			c3 = color3.getRGB();

			p1 = (Point2D.Float) xform.transform(p1, new Point2D.Float());
			p2 = (Point2D.Float) xform.transform(p2, new Point2D.Float());
			p3 = (Point2D.Float) xform.transform(p3, new Point2D.Float());
			// constants for barycentric coords
			x1=p1.x; x2=p2.x; x3=p3.x; y1=p1.y; y2=p2.y; y3=p3.y;
			x23=x2-x3; x13=x1-x3; y23=y2-y3; y13=y1-y3; // x12=x1-x2; y12=y1-y2;
			denom=1f/((y23*x13)-(x23*y13));
			
			this.antialiasing = antialiasing;
		}


		@Override
		public void dispose() {
			if(saved != null)
				cacheRaster(saved);
			saved = null;
		}

		@Override
		public ColorModel getColorModel() {
			return cm;
		}

		@Override
		public Raster getRaster(int xA, int yA, int w, int h) {
			WritableRaster rast = saved;
			if (rast == null) {
				rast = getCachedOrCreateRaster(w, h);
				saved = rast;
			} else if(rast.getWidth() != w || rast.getHeight() != h) {
				int[] data = dataFromRaster(rast);
				if(data.length < w*h) {
					data = new int[w*h];
				}
				rast = createRaster(w, h, data);
				saved = rast;
			}

			// fill data array with interpolated colors (barycentric coords)
			int[] data = dataFromRaster(rast);
			if(antialiasing)
				fillRasterMSAA(xA, yA, w, h, data);
			else
				fillRaster(xA, yA, w, h, data);


			return rast;
		}

		protected void fillRaster(int xA, int yA, int w, int h, int[] data) {
			if(c1==c2&&c2==c3) {
				// all vertices same color
				for(int i=0; i<h; i++) {
					float y = yA+i+.5f;
					float ypart11 = -x23*(y-y3);
					float ypart21 =  x13*(y-y3);
			
					for(int j=0; j<w; j++) {
						float x = xA+j+.5f;
						// calculate barycentric coordinates for (x,y)
						float l1 = ( y23*(x-x3)+ypart11)*denom;
						float l2 = (-y13*(x-x3)+ypart21)*denom;
						float l3 = 1f-l1-l2;
						// determine color
						int mix1;
						if(l1<0||l2<0||l3<0) mix1 = 0;
						else mix1 = c1;
						data[i*w+j] = mix1;
					}
				}
			} else {
				// vertices of different color
				for(int i=0; i<h; i++) {
					float y = yA+i+.5f;
					float ypart11 = -x23*(y-y3);
					float ypart21 =  x13*(y-y3);
			
					for(int j=0; j<w; j++) {
						float x = xA+j+.5f;
						// calculate barycentric coordinates for (x,y)
						float l1 = ( y23*(x-x3)+ypart11)*denom;
						float l2 = (-y13*(x-x3)+ypart21)*denom;
						float l3 = 1f-l1-l2;
						// determine color
						int mix1;
						if(l1<0||l2<0||l3<0) mix1 = 0;
						else mix1 = mixColor3(c1, c2, c3, l1, l2, l3);
						data[i*w+j] = mix1;
					}
				}
			}
		}


		protected void fillRasterMSAA(int xA, int yA, int w, int h, int[] data) {
			final boolean monochrome = c1==c2&&c2==c3;
			for(int i=0; i<h; i++) {
				float y = yA+i+MSAA_SAMPLES[1];
				float ypart11 = -x23*(y-y3);
				float ypart21 =  x13*(y-y3);
				y = yA+i+MSAA_SAMPLES[3];
				float ypart12 = -x23*(y-y3);
				float ypart22 =  x13*(y-y3);
				y = yA+i+MSAA_SAMPLES[5];
				float ypart13 = -x23*(y-y3);
				float ypart23 =  x13*(y-y3);
				y = yA+i+MSAA_SAMPLES[7];
				float ypart14 = -x23*(y-y3);
				float ypart24 =  x13*(y-y3);

				for(int j=0; j<w; j++) {
					float x = xA+j+MSAA_SAMPLES[0];
					float xpart11 =  y23*(x-x3);
					float xpart21 = -y13*(x-x3);
					x = xA+j+MSAA_SAMPLES[2];
					float xpart12 =  y23*(x-x3);
					float xpart22 = -y13*(x-x3);
					x = xA+j+MSAA_SAMPLES[4];
					float xpart13 =  y23*(x-x3);
					float xpart23 = -y13*(x-x3);
					x = xA+j+MSAA_SAMPLES[6];
					float xpart14 =  y23*(x-x3);
					float xpart24 = -y13*(x-x3);

					// calculate barycentric coordinates for the 4 sub pixel samples
					float l11 = (xpart11+ypart11)*denom;
					float l21 = (xpart21+ypart21)*denom;
					float l31 = 1f-l11-l21;

					float l12 = (xpart12+ypart12)*denom;
					float l22 = (xpart22+ypart22)*denom;
					float l32 = 1f-l12-l22;

					float l13 = (xpart13+ypart13)*denom;
					float l23 = (xpart23+ypart23)*denom;
					float l33 = 1f-l13-l23;

					float l14 = (xpart14+ypart14)*denom;
					float l24 = (xpart24+ypart24)*denom;
					float l34 = 1f-l14-l24;

					// determine sample colors and weights (out of triangle samples have 0 weight)
					int mix1,mix2,mix3,mix4;
					float w1,w2,w3,w4;

					if(l11<0||l21<0||l31<0) { mix1 = 0; w1=0f; }
					else { mix1 = monochrome ? c1:mixColor3(c1, c2, c3, l11, l21, l31); w1=1f; }

					if(l12<0||l22<0||l32<0) { mix2 = 0; w2=0f; }
					else { mix2 = monochrome ? c1:mixColor3(c1, c2, c3, l12, l22, l32); w2=1f; }

					if(l13<0||l23<0||l33<0) {mix3 = 0; w3=0f; }
					else { mix3 = monochrome ? c1:mixColor3(c1, c2, c3, l13, l23, l33); w3=1f; }

					if(l14<0||l24<0||l34<0) { mix4 = 0; w4=0f; }
					else { mix4 = monochrome ? c1:mixColor3(c1, c2, c3, l14, l24, l34); w4=1f; }

					int color = mixColor4(mix1, mix2, mix3, mix4, w1,w2,w3,w4);
					data[i*w+j] = scaleColorAlpha(color,(w1+w2+w3+w4)*.25f);
				}
			}
		}

		protected WritableRaster getCachedOrCreateRaster(int w, int h) {
			if(cache != null) {
				int[] data = cache.get();
				if (data != null && data.length >= w*h)
				{
					cache = null;
					return createRaster(w, h, data);
				}
			}
			return createRaster(w, h, new int[w*h]);
		}

		protected void cacheRaster(WritableRaster ras) {
			int[] toCache = dataFromRaster(ras);
			if (cache != null) {
				int[] data = cache.get();
				if (data != null) {
					if (toCache.length < data.length) {
						return;
					}
				}
			}
			cache = new WeakReference<>(toCache);
		}

		protected WritableRaster createRaster(int w, int h, int[] data) {
			DataBufferInt buffer = new DataBufferInt(data, w*h);
			WritableRaster raster = Raster.createPackedRaster(buffer, w, h, w, cm.getMasks(), null);
			return raster;
		}

		private int[] dataFromRaster(WritableRaster wr) {
			return ((DataBufferInt)wr.getDataBuffer()).getData();
		}
		
		private int mixColor3(int c1, int c2, int c3, float m1, float m2, float m3) {
			float normalize = 1f/(m1+m2+m3);
			float a = (a(c1)*m1 + a(c2)*m2 + a(c3)*m3)*normalize;
			float r = (r(c1)*m1 + r(c2)*m2 + r(c3)*m3)*normalize;
			float g = (g(c1)*m1 + g(c2)*m2 + g(c3)*m3)*normalize;
			float b = (b(c1)*m1 + b(c2)*m2 + b(c3)*m3)*normalize;
			return argb((int)a, (int)r, (int)g, (int)b);
		}
		
		private int mixColor4(int c1, int c2, int c3, int c4, float m1, float m2, float m3, float m4) {
			float normalize = 1f/(m1+m2+m3+m4);
			float a = (a(c1)*m1 + a(c2)*m2 + a(c3)*m3 + a(c4)*m4)*normalize;
			float r = (r(c1)*m1 + r(c2)*m2 + r(c3)*m3 + r(c4)*m4)*normalize;
			float g = (g(c1)*m1 + g(c2)*m2 + g(c3)*m3 + g(c4)*m4)*normalize;
			float b = (b(c1)*m1 + b(c2)*m2 + b(c3)*m3 + b(c4)*m4)*normalize;
			return argb((int)a, (int)r, (int)g, (int)b);
		}
		
		private int a(int argb) {
			return (argb >> 24) & 0xff;
		}
		
		private int r(int argb) {
			return (argb >> 16) & 0xff;
		}
		
		private int g(int argb) {
			return (argb >> 8) & 0xff;
		}
		
		private int b(int argb) {
			return (argb) & 0xff;
		}
		
		private int argb(final int a, final int r, final int g, final int b){
			return (a<<24)|(r<<16)|(g<<8)|b;
		}
		
		private int scaleColorAlpha(int color, float m) {
			float normalize = 1f/255f;
			float af = a(color)*normalize*m;
			int a = (((int)(af*255f)) & 0xff) << 24;
			return (color&0x00ffffff)|a;
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
		
		public DefaultGlyph(Consumer<VertexArray> vertGen, int numVerts, int primType, int pixelSize, boolean elements, boolean isFilled, BiFunction<Document,Integer,List<Element>> svgGen, BiFunction<PDPageContentStream,Integer,PDPageContentStream> pdfGen, Graphics2DDrawing fallbackDraw) {
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
		 * @return the global alpha multiplier of the segments in this collection
		 */
		public float getGlobalAlphaMultiplier() {
			return (float)globalAlphaMultiplier.getAsDouble();
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
		
		/**
		 * Whether this Lines object has a stroke pattern other than 0xffff (completely solid).
		 * @return true when stroke pattern != 0xffff
		 */
		public boolean hasStrokePattern() {
			return this.strokePattern != (short)0xffff;
		}


		/**
		 * Returns this {@link Lines} object's stroke pattern
		 * @return stroke pattern
		 */
		public short getStrokePattern() {
			return this.strokePattern;
		}

		/**
		 * Returns the stroke length in pixels, which is by default 16 pixels.
		 * @return stroke length
		 */
		public float getStrokeLength() {
			return strokeLength;
		}
		
		/** @return the saturation multiplier of this renderable */
		public float getGlobalSaturationMultiplier() {
			return (float)globalSaturationMultiplier.getAsDouble();
		}

		
		/**
		 * @return whether vertex rounding is enabled. This indicates if
		 * the {@link LinesRenderer}'s shader will round vertex positions of 
		 * the quad vertices (that a segment is expanded to) to integer values.
		 * <p>
		 * This has the effect of sharpening horizontal and vertical lines, but
		 * can affect differently oriented lines to shrink in thickness or even vanish. 
		 */
		public boolean isVertexRoundingEnabled() {
			return useVertexRounding;
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
		
		/** @return the saturation multiplier of this renderable */
		public float getGlobalSaturationMultiplier() {
			return (float)globalSaturationMultiplier.getAsDouble();
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

		/**
		 * Return true when anti-aliasing is enabled for fallback (AWT) rendering (default=false).
		 * <p>
		 * Especially for triangle grids/meshes this option should be disabled to avoid visible triangle edges
		 * inside the grid/mesh.
		 * When triangles are not connected, this option can be enabled to get anti-aliased edges.
		 * @return true when enabled
		 */
		public boolean isAAinFallbackEnabled() {
			return this.useAAinFallback;
		}
		
		/**
		 * Returns the triangles that intersect or contain the specified rectangle.
		 * @param rect rectangle to test intersection
		 * @return list of intersecting triangles
		 */
		public List<TriangleDetails> getIntersectingTriangles(Rectangle2D rect){
			boolean useParallelStreaming = numTriangles() > 1000;
			return Utils.parallelize(getTriangleDetails().stream(), useParallelStreaming)
					.filter(tri->Utils.rectIntersectsOrIsContainedInTri(
							rect, 
							tri.p0.getX(), tri.p0.getY(), 
							tri.p1.getX(), tri.p1.getY(), 
							tri.p2.getX(), tri.p2.getY()
							))
					.collect(Collectors.toList());
		}

		/**
		 * Removes all TriangleDetails from this collection.
		 * Sets the {@link #isDirty()} state to true.
		 * @return this for chaining
		 */
		public Triangles removeAllTriangles() {
			triangles.clear();
			setDirty();
			return this;
		}
		
		/**
		 * Sets the {@link #isDirty()} state of this {@link Triangles} object to true.
		 * @return this for chaining
		 */
		public Triangles setDirty() {
			this.isDirty = true;
			return this;
		}


	}
		

	
	/**
	 * Specification of a line segment which comprises vertex locations, colors, picking color, and thicknesses.
	 * @author hageldave
	 */
	public class SegmentDetails implements Cloneable {
		protected final DoubleSupplier[] PREDEFINED_THICKNESSES = new DoubleSupplier[]
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
		
		protected DoubleSupplier sup4thick(double t){
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
	public class TriangleDetails implements Cloneable {
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
	 * Packs 8bit RGB color components into a single 32bit ARGB integer value
	 * with alpha=255 (opaque).
	 * Components larger than 8bit are NOT truncated and will result in a
	 * broken, malformed value.
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #argb(int, int, int, int)
	 * @see #argb_bounded(int, int, int, int)
	 * @see #argb_fast(int, int, int, int)
	 * @see #rgb_bounded(int, int, int)
	 * @see #rgb(int, int, int)
	 * @see #a(int)
	 * @see #r(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @since 1.0
	 */
	public static final int rgb_fast(final int r, final int g, final int b){
		return 0xff000000|(r<<16)|(g<<8)|b;
	}

	
	/**
	 * Packs 8bit RGB color components into a single 32bit ARGB integer value
	 * with alpha=255 (opaque).
	 * Components are clamped to [0,255].
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #argb(int, int, int, int)
	 * @see #argb_bounded(int, int, int, int)
	 * @see #argb_fast(int, int, int, int)
	 * @see #rgb(int, int, int)
	 * @see #rgb_fast(int, int, int)
	 * @see #a(int)
	 * @see #r(int)
	 * @see #g(int)
	 * @see #b(int)
	 * @since 1.0
	 */
	public static final int rgb_bounded(final int r, final int g, final int b){
		return rgb_fast(
				r > 255 ? 255: r < 0 ? 0:r,
				g > 255 ? 255: g < 0 ? 0:g,
				b > 255 ? 255: b < 0 ? 0:b);
	}


	/**
	 * Packs normalized ARGB color components (values in [0.0 .. 1.0]) into a
	 * single 32bit integer value with alpha=255 (opaque).
	 * Component values less than 0 or greater than 1 clamped to fit the range.
	 * @param r red
	 * @param g green
	 * @param b blue
	 * @return packed ARGB value
	 *
	 * @see #argb_fromNormalized(double, double, double, double)
	 * @see #rgb(int, int, int)
	 * @see #a_normalized(int)
	 * @see #r_normalized(int)
	 * @see #g_normalized(int)
	 * @see #b_normalized(int)
	 * @since 1.2
	 */
	public static final int rgb_fromNormalized(final double r, final double g, final double b){
		return rgb_bounded((int)Math.round(r*0xff), (int)Math.round(g*0xff), (int)Math.round(b*0xff));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						cntrLineSegments.add(cp.new SegmentDetails(new Point2D.Double(x0, y0), new Point2D.Double(x1, y1)).setColor(color_));
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
						tris.add(cp.new TriangleDetails(tx0,ty0, tx1,ty1, tx2,ty2)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, x0,y0, x1,y1)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, x0,y0, x1,y1)
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
						tris.add(cp.new TriangleDetails(x0,y0, tx1,ty1, x1,y1)
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
						tris.add(cp.new TriangleDetails(x0,y0, tx1,ty1, x1,y1)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, tx2,ty2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, tx2,ty2)
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
						tris.add(cp.new TriangleDetails(tx1,ty1, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
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
						tris.add(cp.new TriangleDetails(tx1,ty1, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c1_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, tx2,ty2, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v2)))
								.setColor2(c2_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx2,ty2, x0,y0)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, tx1,ty1, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c1_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx1,ty1, x0,y0)
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
						tris.add(cp.new TriangleDetails(tx0,ty0, tx1,ty1, x0,y0)
								.setColor0(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v0)))
								.setColor1(interpolateColor(c1, c2, interpolateToValue(isoValue1, isoValue2, v1)))
								.setColor2(c2_)
						);
						tris.add(cp.new TriangleDetails(x1,y1, tx1,ty1, x0,y0)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx1,ty1)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx0,ty0)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx0,ty0)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx1,ty1)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx2,ty2)
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
						tris.add(cp.new TriangleDetails(x0,y0, x1,y1, x2,y2)
								.setColor0(c1_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x1,y1, x2,y2)
								.setColor0(c2_)
								.setColor1(c2_)
								.setColor2(c1_));
						tris.add(cp.new TriangleDetails(x3,y3, x2,y2, tx2,ty2)
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
	
	public abstract class GenericRenderer<T extends Renderable> implements Renderer, AdaptableView {
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
		
		@Override
	    public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
	        if (!isEnabled()) {
	            return;
	        }

	        double translateX = Objects.isNull(view) ? 0 : view.getX();
	        double translateY = Objects.isNull(view) ? 0 : view.getY();
	        double scaleX = Objects.isNull(view) ? 1 : w / view.getWidth();
	        double scaleY = Objects.isNull(view) ? 1 : h / view.getHeight();

	        Rectangle2D viewportRect = new Rectangle2D.Float(0, 0, w, h);
	        float[][] polygonCoords = new float[2][4];

	        for (Lines lines : getItemsToRender()) {
	            if (lines.isHidden() || lines.getStrokePattern() == 0 || lines.numSegments() == 0) {
	                // line is invisible
	                continue;
	            }

	            boolean hasVaryingThickness = false;
	            double thick = lines.getSegments().get(0).thickness0.getAsDouble();
	            for (int i = 0; i < lines.numSegments(); i++) {
	                SegmentDetails seg = lines.getSegments().get(i);
	                if (seg.thickness0.getAsDouble() != thick || seg.thickness1.getAsDouble() != thick) {
	                    hasVaryingThickness = true;
	                    break;
	                }
	            }

	            if (hasVaryingThickness)
	                renderFallbackLinesVT(g, p, lines, translateX, translateY, scaleX, scaleY, viewportRect, polygonCoords);
	            else
	                renderFallbackLinesCT(g, p, lines, translateX, translateY, scaleX, scaleY, viewportRect, (float) ( thick * lines.getGlobalThicknessMultiplier() ));

	        }
	    }
		
		private void renderFallbackLinesCT(
	            Graphics2D g,
	            Graphics2D p,
	            Lines lines,
	            double translateX,
	            double translateY,
	            double scaleX,
	            double scaleY,
	            Rectangle2D viewportRect,
	            float thickness) {
	        double dist = 0;
	        double prevX = 0;
	        double prevY = 0;

	        float[] dash = lines.hasStrokePattern() ? strokePattern2dashPattern(lines.getStrokePattern(), lines.getStrokeLength()) : null;

	        for (SegmentDetails seg : lines.getSegments()) {
	            double x1, y1, x2, y2;
	            x1 = seg.p0.getX();
	            y1 = seg.p0.getY();
	            x2 = seg.p1.getX();
	            y2 = seg.p1.getY();

	            x1 -= translateX;
	            x2 -= translateX;
	            y1 -= translateY;
	            y2 -= translateY;
	            x1 *= scaleX;
	            x2 *= scaleX;
	            y1 *= scaleY;
	            y2 *= scaleY;

	            // path length calculations
	            double dx = x2 - x1;
	            double dy = y2 - y1;
	            double len = Utils.hypot(dx, dy);
	            double l1;
	            if (prevX == x1 && prevY == y1) {
	                l1 = dist;
	                dist += len;
	                dist = dist % lines.getStrokeLength();
	            } else {
	                l1 = 0;
	                dist = len;
	            }
	            prevX = x2;
	            prevY = y2;

	            if (lines.isVertexRoundingEnabled()) {
	                x1 = (int) ( x1 + 0.5 );
	                x2 = (int) ( x2 + 0.5 );
	                y1 = (int) ( y1 + 0.5 );
	                y2 = (int) ( y2 + 0.5 );
	                if (thickness % 2 == 1f) {
	                    x1 += .5f;
	                    x2 += .5f;
	                    y1 += .5f;
	                    y2 += .5f;
	                }
	            }

	            // visibility check
	            if (!viewportRect.intersectsLine(x1, y1, x2, y2)) {
	                continue;
	            }

	            Paint paint;

	            int c1 = ColorOperations.changeSaturation(seg.color0.getAsInt(), lines.getGlobalSaturationMultiplier());
	            c1 = ColorOperations.scaleColorAlpha(c1, lines.getGlobalAlphaMultiplier());

	            int c2 = ColorOperations.changeSaturation(seg.color1.getAsInt(), lines.getGlobalSaturationMultiplier());
	            c2 = ColorOperations.scaleColorAlpha(c2, lines.getGlobalAlphaMultiplier());

	            if (c1 != c2) {
	                paint = new GradientPaint((float) x1, (float) y1, new Color(c1, true), (float) x2, (float) y2, new Color(c2, true));
	            } else paint = new Color(c1, true);
	            g.setPaint(paint);

	            BasicStroke stroke;
	            if (lines.hasStrokePattern()) {
	                stroke = new BasicStroke(thickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1f, dash, (float) l1);
	            } else {
	                stroke = new BasicStroke(thickness, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 1f);
	            }
	            g.setStroke(stroke);
	            g.draw(new Line2D.Double(x1, y1, x2, y2));

	            if (seg.pickColor != 0) {
	                p.setStroke(stroke);
	                p.setColor(new Color(seg.pickColor));
	                p.draw(new Line2D.Double(x1, y1, x2, y2));
	            }

	        }
	    }

	    private void renderFallbackLinesVT(
	            Graphics2D g,
	            Graphics2D p,
	            Lines lines,
	            double translateX,
	            double translateY,
	            double scaleX,
	            double scaleY,
	            Rectangle2D viewportRect,
	            float[][] polygonCoords) {
	        double dist = 0;
	        double prevX = 0;
	        double prevY = 0;

	        for (SegmentDetails seg : lines.getSegments()) {
	            double x1, y1, x2, y2;
	            x1 = seg.p0.getX();
	            y1 = seg.p0.getY();
	            x2 = seg.p1.getX();
	            y2 = seg.p1.getY();

	            x1 -= translateX;
	            x2 -= translateX;
	            y1 -= translateY;
	            y2 -= translateY;
	            x1 *= scaleX;
	            x2 *= scaleX;
	            y1 *= scaleY;
	            y2 *= scaleY;

	            // path length calculations
	            double dx = x2 - x1;
	            double dy = y2 - y1;
	            double len = Utils.hypot(dx, dy);
	            double l1, l2;
	            if (prevX == x1 && prevY == y1) {
	                l1 = dist;
	                l2 = dist + len;
	                dist += len;
	                dist = dist % lines.getStrokeLength();
	            } else {
	                l1 = 0;
	                l2 = len;
	                dist = len;
	            }
	            prevX = x2;
	            prevY = y2;

	            // visibility check
	            if (!viewportRect.intersectsLine(x1, y1, x2, y2)) {
	                continue;
	            }

	            // miter vector stuff
	            double normalize = 1 / len;
	            double miterX = dy * normalize * 0.5;
	            double miterY = -dx * normalize * 0.5;
	            double t1 = seg.thickness0.getAsDouble() * lines.getGlobalThicknessMultiplier();
	            double t2 = seg.thickness1.getAsDouble() * lines.getGlobalThicknessMultiplier();


	            Paint paint;

	            int c1 = ColorOperations.changeSaturation(seg.color0.getAsInt(), lines.getGlobalSaturationMultiplier());
	            c1 = ColorOperations.scaleColorAlpha(c1, lines.getGlobalAlphaMultiplier());

	            int c2 = ColorOperations.changeSaturation(seg.color1.getAsInt(), lines.getGlobalSaturationMultiplier());
	            c2 = ColorOperations.scaleColorAlpha(c2, lines.getGlobalAlphaMultiplier());

	            if (c1 != c2) {
	                paint = new GradientPaint((float) x1, (float) y1, new Color(c1, true), (float) x2, (float) y2, new Color(c2, true));
	            } else paint = new Color(c1, true);
	            g.setPaint(paint);

	            if (!lines.hasStrokePattern()) {
	                float[][] pc = polygonCoords;
	                pc[0][0] = (float) ( x1 + miterX * t1 );
	                pc[1][0] = (float) ( y1 + miterY * t1 );
	                pc[0][1] = (float) ( x2 + miterX * t2 );
	                pc[1][1] = (float) ( y2 + miterY * t2 );
	                pc[0][2] = (float) ( x2 - miterX * t2 );
	                pc[1][2] = (float) ( y2 - miterY * t2 );
	                pc[0][3] = (float) ( x1 - miterX * t1 );
	                pc[1][3] = (float) ( y1 - miterY * t1 );
	                // vertex rounding
	                /*if (lines.isVertexRoundingEnabled()) {
	                    for (int i = 0; i < 4; i++) {
	                        pc[0][i] = (int) ( pc[0][i] + .5f );
	                        pc[1][i] = (int) ( pc[1][i] + .5f );
	                    }
	                }*/
	                // drawing
	                int pci[][] = new int[2][4];
	                for (int i = 0; i < 4; i++) {
	                    pci[0][i] = (int)(pc[0][i] + 0.5f);
	                    pci[1][i] = (int)(pc[1][i] + 0.5f);
	                }
	                g.fillPolygon(pci[0], pci[1], 4);
	                if (seg.pickColor != 0) {
	                    p.setColor(new Color(seg.pickColor));
	                    p.fillPolygon(pci[0], pci[1], 4);
	                }
	            } else {
	                float[][] pc = polygonCoords;
	                double[] strokeInterval = findStrokeInterval(l1, lines.getStrokeLength(), lines.getStrokePattern());
	                while (strokeInterval[0] < l2) {
	                    double start = strokeInterval[0];
	                    double end = Math.min(strokeInterval[1], l2);
	                    // interpolation factors
	                    double m1 = Math.max(( start - l1 ) / ( l2 - l1 ), 0);
	                    double m2 = ( end - l1 ) / ( l2 - l1 );
	                    // interpolate miters
	                    double t1_ = t1 * ( 1 - m1 ) + t2 * m1;
	                    double t2_ = t1 * ( 1 - m2 ) + t2 * m2;
	                    // interpolate segment
	                    double x1_ = x1 + dx * m1;
	                    double x2_ = x1 + dx * m2;
	                    double y1_ = y1 + dy * m1;
	                    double y2_ = y1 + dy * m2;

	                    pc[0][0] = (float) ( x1_ + miterX * t1_ );
	                    pc[1][0] = (float) ( y1_ + miterY * t1_ );
	                    pc[0][1] = (float) ( x2_ + miterX * t2_ );
	                    pc[1][1] = (float) ( y2_ + miterY * t2_ );
	                    pc[0][2] = (float) ( x2_ - miterX * t2_ );
	                    pc[1][2] = (float) ( y2_ - miterY * t2_ );
	                    pc[0][3] = (float) ( x1_ - miterX * t1_ );
	                    pc[1][3] = (float) ( y1_ - miterY * t1_ );
	                    // vertex rounding
	                    /*if (lines.isVertexRoundingEnabled()) {
	                        for (int i = 0; i < 4; i++) {
	                            pc[0][i] = (int) ( pc[0][i] + .5f );
	                            pc[1][i] = (int) ( pc[1][i] + .5f );
	                        }
	                    }*/
	                    // drawing
	                    int pci[][] = new int[2][4];
		                for (int i = 0; i < 4; i++) {
		                    pci[0][i] = (int)(pc[0][i] + 0.5f);
		                    pci[1][i] = (int)(pc[1][i] + 0.5f);
		                }
		                g.fillPolygon(pci[0], pci[1], 4);
		                if (seg.pickColor != 0) {
		                    p.setColor(new Color(seg.pickColor));
		                    p.fillPolygon(pci[0], pci[1], 4);
		                }

	                    strokeInterval = findStrokeInterval(strokeInterval[2], lines.getStrokeLength(), lines.getStrokePattern());
	                }
	            }
	        }
	    }
	    
	    protected double[] findStrokeInterval(double current, double strokeLen, short pattern) {
	        double patternStart = current - ( current % strokeLen );
	        double patternPos = ( current % strokeLen ) * ( 16 / strokeLen );
	        int bit = (int) patternPos;
	        int steps = bit;
	        int[] pat = transferBits(pattern, new int[16]);
	        // find next part of stroke pattern that is solid
	        while (pat[bit] != 1) {
	            bit = ( bit + 1 ) & 0xf;//%16;
	            steps++;
	        }
	        double intervalStart = steps == 0 ? current : patternStart + steps * ( strokeLen / 16 );
	        // find next part of stroke pattern that is empty
	        while (pat[bit] == 1) {
	            bit = ( bit + 1 ) & 0xf;//%16;
	            steps++;
	        }
	        double intervalEnd = patternStart + steps * ( strokeLen / 16 );
	        // find next solid again
	        while (pat[bit] != 1) {
	            bit = ( bit + 1 ) & 0xf;//%16;
	            steps++;
	        }
	        double nextIntervalStart = patternStart + steps * ( strokeLen / 16 );
	        return new double[]{intervalStart, intervalEnd, nextIntervalStart};
	    }

	    
	    protected int[] transferBits(short bits, int[] target) {
	        for (int i = 0; i < 16; i++) {
	            target[15 - i] = ( bits >> i ) & 0b1;
	        }
	        return target;
	    }

	    
	    protected float[] strokePattern2dashPattern(short pattern, float strokeLen) {
	        int[] bits = transferBits(pattern, new int[16]);
	        // shift pattern to a valid start
	        while (bits[0] != 1 && bits[15] != 0) {
	            int b0 = bits[0];
	            for (int i = 0; i < 15; i++)
	                bits[i] = bits[i + 1];
	            bits[15] = b0;
	        }

	        float unit = strokeLen / 16f;
	        int currentBit = bits[0];
	        int currentLen = 1;
	        int iDash = 0;
	        float[] dash = new float[16];
	        for (int i = 1; i < 16; i++) {
	            if (currentBit == bits[i]) {
	                currentLen++;
	            } else {
	                dash[iDash++] = currentLen * unit;
	                currentLen = 1;
	                currentBit = bits[i];
	            }
	            if (i == 15)
	                dash[iDash] = currentLen * unit;
	        }
	        return Arrays.copyOf(dash, iDash + 1);
	    }




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
		
		@Override
		public void renderFallback(Graphics2D g, Graphics2D p, int w, int h) {
			if(!isEnabled()){
				return;
			}
			
			double translateX = Objects.isNull(view) ? 0:view.getX();
			double translateY = Objects.isNull(view) ? 0:view.getY();
			double scaleX = Objects.isNull(view) ? 1:w/view.getWidth();
			double scaleY = Objects.isNull(view) ? 1:h/view.getHeight();
			
			Rectangle vpRect = new Rectangle(w, h);
			
			for(Text txt: getItemsToRender()){
				if(txt.isHidden() || txt.getTextString().isEmpty()){
					continue;
				}
				{
					double x1,y1,angle;
					x1 = txt.getOrigin().getX();
					y1 = txt.getOrigin().getY();
					angle = txt.getAngle();
					
					x1-=translateX;
					y1-=translateY;
					x1*=scaleX;
					y1*=scaleY;
					
					y1+=1;
					
					double effectiveTx = x1-txt.getOrigin().getX();
					double effectiveTy = y1-txt.getOrigin().getY();
					// test if inside of view port
					Rectangle2D txtrect = txt.getBoundsWithRotation();
					txtrect.setRect(
							txtrect.getX()+effectiveTx, 
							txtrect.getY()+effectiveTy, 
							txtrect.getWidth(), txtrect.getHeight()
					);
					
					if(!txtrect.intersects(vpRect)) {
						continue;
					}
					// create a proxy graphics object to draw the string to
					Graphics2D g_ = (Graphics2D) g.create();
					Graphics2D p_ = (Graphics2D) p.create();
					
					//Font font = FontProvider.getUbuntuMono(txt.fontsize, txt.style);
					Font font = MipavUtil.font12;
					g_.setFont(font);
					p_.setFont(font);
					
					/* translate to text origin, 
					 * flip vertically (AWT coordinates, so text is not upside down), 
					 * rotate according to angle */
					AffineTransform trnsfrm = new AffineTransform();
					trnsfrm.translate(x1, y1);
					trnsfrm.scale(1, -1);
					if(angle != 0.0)
						trnsfrm.rotate(-angle);
					g_.transform(trnsfrm);
					p_.transform(trnsfrm);
					
					// draw background rectangle
					if(txt.getBackground().getRGB() != 0){
						g_.setColor(txt.getBackground());
						Rectangle2D bounds = txt.getBounds();
						float rightpadding = 0.4f*((float)bounds.getWidth()/txt.getTextString().length());
						Rectangle2D rect = new Rectangle2D.Double(0.0, -bounds.getHeight(), bounds.getWidth()+rightpadding, bounds.getHeight());
						g_.fill(rect);
					}
					// draw string
					int maxDescent = g_.getFontMetrics().getMaxDescent();
					g_.setColor(txt.getColor());
					g_.drawString(txt.getTextString(), 0, -maxDescent);
					
					if(txt.getPickColor() != 0) {
						p_.setColor(new Color(txt.getPickColor()));
						Rectangle2D bounds = txt.getBounds();
						float rightpadding = 0.4f*((float)bounds.getWidth()/txt.getTextString().length());
						Rectangle2D rect = new Rectangle2D.Double(0.0, -bounds.getHeight(), bounds.getWidth()+rightpadding, bounds.getHeight());
						p_.fill(rect);
					}
				}
			}
		}

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
	 * The AdaptableView interface defines the {@link #setView(Rectangle2D)}
	 * method.
	 * An implementing class of this interface is meant to be able to adjust its
	 * view based on the view rectangle passed to the method.
	 * This is implemented for example by {@link GenericRenderer}.
	 * 
	 * @author hageldave
	 */
	public interface AdaptableView {

		/**
		 * Sets the view rectangle that is the range of coordinates to be
		 * projected onto the view port.
		 * <p>
		 * For example when setting a view rectangle of (x=-1,y=-1,w=2,h=2)
		 * given a viewport of size (w=100,h=100), then a point with coordinates
		 * (x=0,y=0) will be projected to (x=50,y=50) on the viewport.
		 * A point with coordinates (x=-1,y=-1) will be projected to (x=0,y=0).
		 * <p>
		 * When setting the view rectangle to null, then no projection is happening
		 * and the coordinates are mapped directly to view port coordinates.
		 * E.g. coordinates (40,40) will be (40,40) on the viewport, as if the
		 * view rectangles size was coupled to the viewport size.
		 * 
		 * @param view the view rectangle (can be null)
		 */
		public void setView(Rectangle2D view);
		
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
				if(content instanceof AdaptableView){
					((AdaptableView) content).setView(coordinateView);
				}
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
				if(content instanceof AdaptableView){
					((AdaptableView) content).setView(coordinateView);
				}
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

		/**
		 * Sets the coordinate view. This is the range of x and y coordinates that is displayed by this
		 * {@link CoordSysRenderer}. It is not the rectangular area in which the content appears on screen
		 * but what coordinates that area corresponds to as a coordinate system.
		 * <p>
		 * This determines what range of coordinates is visible when rendering the {@link #content}.
		 * By default the coordinate view covers the range [-1,1] for both x and y coordinates.
		 * When the resulting value ranges maxX-minX or maxY-minY fall below 1e-9, the method
		 * refuses to set the view accordingly to prevent unrecoverable zooming and inaccurate
		 * or broken renderings due to floating point precision.
		 * <p>
		 * This method also sets the {@link #isDirty} state of this {@link CoordSysRenderer} to true.
		 * <p>
		 * When {@link CoordinateViewListener} are registered to this renderer, they will be notified
		 * before this method returns.
		 * 
		 * @param minX minimum x coordinate visible in the coordinate system
		 * @param minY minimum y coordinate visible in the coordinate system
		 * @param maxX maximum x coordinate visible in the coordinate system
		 * @param maxY maximum y coordinate visible in the coordinate system
		 * @return this for chaining
		 */
		public CoordSysRenderer setCoordinateView(double minX, double minY, double maxX, double maxY){
			return setCoordinateViewRect(minX, minY, maxX-minX, maxY-minY);
		}
		
		/**
		 * Sets the coordinate view. This is the range of x and y coordinates that is displayed by this
		 * {@link CoordSysRenderer}. It is not the rectangular area in which the content appears on screen
		 * but what coordinates that area corresponds to as a coordinate system.
		 * <p>
		 * See also {@link #setCoordinateView(double, double, double, double)}.
		 * 
		 * @param viewRect to set the view to
		 * @return this for chaining
		 */
		public CoordSysRenderer setCoordinateView(Rectangle2D viewRect){
			return setCoordinateViewRect(viewRect.getMinX(), viewRect.getMinY(), viewRect.getWidth(), viewRect.getHeight());
		}
		
		protected CoordSysRenderer setCoordinateViewRect(double x, double y, double w, double h) {
			if(w < 1e-14 || h < 1e-14){
				System.err.printf("hitting coordinate area precision limit, x-range:%e, y-range:%e%n", w, h);
				return this;
			}
			this.coordinateView = new Rectangle2D.Double(x, y, w, h);
			setDirty();
			if(Objects.nonNull(coordviewListener)){
				coordviewListener.actionPerformed(new ActionEvent(this, ActionEvent.ACTION_FIRST, "setCoordinateView"));
			}
			return this;
		}

		/**
		 * Adds a {@link CoordinateViewListener} to this renderer which will be notified
		 * whenever the coordinate view changes 
		 * (i.e. when {@link #setCoordinateView(double, double, double, double)} is called)
		 * @param l listener
		 * @return this for chaining
		 */
		synchronized public CoordSysRenderer addCoordinateViewListener(CoordinateViewListener l){
			if(l==null)
				return this;
			coordviewListener = AWTEventMulticaster.add(coordviewListener, l);
			return this;
		}

		/**
		 * Removes the specified {@link CoordinateViewListener} from this renderer.
		 * @param l listener
		 * @return this for chaining
		 */
		synchronized public CoordSysRenderer removeActionListener(CoordinateViewListener l){
			if(l==null)
				return this;
			coordviewListener = AWTEventMulticaster.remove(coordviewListener, l);
			return this;
		}

		/**
		 * Returns the coordinate view, which is the range of x and y coordinates visible in the
		 * coordinate system.
		 * See {@link #setCoordinateView(double, double, double, double)}.
		 * @return the coordinate view
		 */
		public Rectangle2D getCoordinateView() {
			return coordinateView;
		}
        
		/**
		 * Sets the content renderer that will draw into the area of the coordinate system.
		 * @param content the content renderer
		 * @return the previous content renderer (which may need to be closed to free GL resources),
		 * null if none was set
		 */
		public Renderer setContent(Renderer content) {
			Renderer old = this.content;
			this.content = content;
			return old;
		}


	}
	
	/**
	 * The CoordinateViewListener is a listener that listens on changes to a 
	 * {@link CoordSysRenderer}'s coordinate view 
	 * (i.e. {@link CoordSysRenderer#setCoordinateView(double, double, double, double)}).
	 * 
	 * @author hageldave
	 */
	public interface CoordinateViewListener extends ActionListener {

		@Override
		default void actionPerformed(ActionEvent e) {
			if(e.getSource() instanceof CoordSysRenderer){
				CoordSysRenderer renderer = (CoordSysRenderer) e.getSource();
				coordinateViewChanged(renderer,renderer.getCoordinateView());
			}
		}

		/**
		 * Method will be called when the source's coordinate view changed.
		 * @param src source {@link CoordSysRenderer}
		 * @param view the new coordinate view
		 */
		void coordinateViewChanged(CoordSysRenderer src, Rectangle2D view);

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
		
		/**
		 * @return the String this text object displays
		 */
		public String getTextString(){
			return txtStr;
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
	
	/**
	 * Basic interface for Pixel classes of {@link ImgBase} implementations.
	 * <br>
	 * An implementation of PixelBase stores a position and works like a pointer into the
	 * data of an image.
	 * An instance of PixelBase (or any implementation of it) is NOT the pixel value of an image at a
	 * specific location and changing its position will not change that image.
	 * Instead an instance is used to iterate the pixel values of an image. Such a pixel value can be 
	 * retrieved and manipulated as long as the instance points to it.
	 * 
	 * @author hageldave
	 * @since 2.0
	 */
	public interface PixelBase {

		/**
		 * Returns the alpha value of this pixel at its current position. 
		 * If the underlying image does not support alpha, 1.0 is returned.
		 * @return alpha value of this pixel with 0.0 as fully transparent and 1.0 as fully opaque.
		 * May exceed [0,1] range depending on implementation.
		 * 
		 * @see #setA_fromDouble(double)
		 * @see #r_asDouble
		 * @see #g_asDouble
		 * @see #b_asDouble
		 */
		public double a_asDouble();

		/**
		 * Returns the red value of this pixel at its current position.
		 * @return red value of this pixel with 0.0 as no red contribution and 1.0 as full red contribution.
		 * May exceed [0,1] range depending on implementation.
		 * 
		 * @see #setR_fromDouble(double)
		 * @see #a_asDouble
		 * @see #g_asDouble
		 * @see #b_asDouble
		 */
		public double r_asDouble();

		/**
		 * Returns the green value of this pixel at its current position.
		 * @return green value of this pixel with 0.0 as no green contribution and 1.0 as full green contribution.
		 * May exceed [0,1] range depending on implementation.
		 * 
		 * @see #setG_fromDouble(double)
		 * @see #a_asDouble
		 * @see #r_asDouble
		 * @see #b_asDouble
		 */
		public double g_asDouble();

		/**
		 * Returns the blue value of this pixel at its current position.
		 * @return blue value of this pixel with 0.0 as no blue contribution and 1.0 as full blue contribution.
		 * May exceed [0,1] range depending on implementation.
		 * 
		 * @see #setB_fromDouble(double)
		 * @see #a_asDouble
		 * @see #r_asDouble
		 * @see #g_asDouble
		 */
		public double b_asDouble();

		/**
		 * Sets the alpha value of this pixel at its current position with 0.0 as fully transparent and 1.0 as fully opaque.
		 * If the underlying image does not support alpha, this call imediately returns without modifying the image. 
		 * @param a the alpha value. May exceed [0,1] range depending on implementation.
		 * @return this pixel for chaining.
		 * 
		 * @see #a_asDouble()
		 * @see #setR_fromDouble(double)
		 * @see #setG_fromDouble(double)
		 * @see #setB_fromDouble(double)
		 * @see #setARGB_fromDouble(double, double, double, double)
		 */
		public PixelBase setA_fromDouble(double a);

		/**
		 * Sets the red value of this pixel at its current position with 0.0 as no red contribution and 1.0 as full red contribution. 
		 * @param r the red value. May exceed [0,1] range depending on implementation.
		 * @return this pixel for chaining.
		 * 
		 * @see #r_asDouble()
		 * @see #setA_fromDouble(double)
		 * @see #setG_fromDouble(double)
		 * @see #setB_fromDouble(double)
		 * @see #setARGB_fromDouble(double, double, double, double)
		 */
		public PixelBase setR_fromDouble(double r);

		/**
		 * Sets the green value of this pixel at its current position with 0.0 as no green contribution and 1.0 as full green contribution. 
		 * @param g the green value. May exceed [0,1] range depending on implementation.
		 * @return this pixel for chaining.
		 * 
		 * @see #g_asDouble()
		 * @see #setA_fromDouble(double)
		 * @see #setR_fromDouble(double)
		 * @see #setB_fromDouble(double)
		 * @see #setARGB_fromDouble(double, double, double, double)
		 */
		public PixelBase setG_fromDouble(double g);

		/**
		 * Sets the blue value of this pixel at its current position with 0.0 as no blue contribution and 1.0 as full blue contribution. 
		 * @param b the blue value. May exceed [0,1] range depending on implementation.
		 * @return this pixel for chaining.
		 * 
		 * @see #b_asDouble()
		 * @see #setA_fromDouble(double)
		 * @see #setR_fromDouble(double)
		 * @see #setG_fromDouble(double)
		 * @see #setARGB_fromDouble(double, double, double, double)
		 */
		public PixelBase setB_fromDouble(double b);

		/**
		 * Sets the alpha, red, green and blue value of this pixel.
		 * Assumed minimum and maximum value for each channel is 0.0 and 1.0
		 * but values may exceed [0,1] range depending on implementation.
		 * @param a alpha value (0=transparent, 1=opaque)
		 * @param r red value
		 * @param g green value
		 * @param b blue value
		 * @return this pixel for chaining.
		 * 
		 * @see #setRGB_fromDouble_preserveAlpha(double, double, double)
		 * @see #setRGB_fromDouble(double, double, double)
		 */
		public default PixelBase setARGB_fromDouble(double a, double r, double g, double b){
			setA_fromDouble(a);
			setR_fromDouble(r);
			setG_fromDouble(g);
			setB_fromDouble(b);
			return this;
		}

		/**
		 * Sets the red, green and blue value of this pixel. 
		 * The alpha value is set to 1.0 (opaque) if the underlying image supports alpha.
		 * Assumed minimum and maximum value for each channel is 0.0 and 1.0
		 * but values may exceed [0,1] range depending on implementation.
		 * @param r red value
		 * @param g green value
		 * @param b blue value
		 * @return this pixel for chaining.
		 * 
		 * @see #setARGB_fromDouble(double, double, double, double)
		 * @see #setRGB_fromDouble_preserveAlpha(double, double, double)
		 */
		public default PixelBase setRGB_fromDouble(double r, double g, double b){
			return setARGB_fromDouble(1.0, r, g, b);
		}

		/**
		 * Sets the red, green and blue value of this pixel. 
		 * The alpha value is preserved.
		 * Assumed minimum and maximum value for each channel is 0.0 and 1.0
		 * but values may exceed [0,1] range depending on implementation.
		 * @param r red value
		 * @param g green value
		 * @param b blue value
		 * @return this pixel for chaining.
		 * 
		 * @see #setARGB_fromDouble(double, double, double, double)
		 * @see #setRGB_fromDouble(double, double, double)
		 */
		public default PixelBase setRGB_fromDouble_preserveAlpha(double r, double g, double b){
			setR_fromDouble(r);
			setG_fromDouble(g);
			setB_fromDouble(b);
			return this;
		}

		/**
		 * @return the x coordinate of this pixel's current position
		 * 
		 * @see #getY()
		 * @see #getXnormalized()
		 * @see #setPosition(int, int)
		 * @see #getIndex()
		 * @see #setIndex(int)
		 */
		public int getX();

		/**
		 * @return the y coordinate of this pixel's current position
		 * 
		 * @see #getX()
		 * @see #getYnormalized()
		 * @see #setPosition(int, int)
		 * @see #getIndex()
		 * @see #setIndex(int)
		 */
		public int getY();

		/**
		 * Returns the normalized x coordinate of this Pixel.
		 * This will return 0 for Pixels at the left boundary and 1 for Pixels
		 * at the right boundary of the Img.<br>
		 * <em>For Img's that are only 1 Pixel wide, <u>NaN</u> is returned.</em>
		 * @return normalized x coordinate within [0..1]
		 * 
		 * @see #getX()
		 * @see #getYnormalized()
		 * @see #setPosition(int, int)
		 * @see #getIndex()
		 * @see #setIndex(int)
		 */
		public default double getXnormalized(){
			return getX()*1.0/(getSource().getWidth()-1.0);
		}

		/**
		 * Returns the normalized y coordinate of this Pixel.
		 * This will return 0 for Pixels at the upper boundary and 1 for Pixels
		 * at the lower boundary of the Img.<br>
		 * <em>For Img's that are only 1 Pixel high, <u>NaN</u> is returned.</em>
		 * @return normalized y coordinate within [0..1]
		 * 
		 * @see #getY()
		 * @see #getXnormalized()
		 * @see #setPosition(int, int)
		 * @see #getIndex()
		 * @see #setIndex(int)
		 */
		public default double getYnormalized(){
			return getY()*1.0/(getSource().getHeight()-1.0);
		}

		/**
		 * Returns this pixel's index. 
		 * The index relates to the pixels's position in the following way:<br>
		 * <pre>
		 * index = y * width + x
		 *     y = index / width
		 *     x = index % width
		 * </pre>
		 * @return index of this pixel
		 * 
		 * @see #setIndex(int)
		 * @see #getX()
		 * @see #getY()
		 */
		public int getIndex();

		/**
		 * Sets the index of this pixel and thus its position.
		 * The index relates to the pixels's position in the following way:<br>
		 * <pre>
		 * index = y * width + x
		 *     y = index / width
		 *     x = index % width
		 * </pre>
		 * Index values outside the range [0, number of pixels] are not allowed
		 * but not necessarily enforced either. Accessing image data with an index/position
		 * outside of the underlying image's boundaries may cause an Exception.
		 * @param index to set the pixel to
		 * @return this pixel for chaining
		 * 
		 * @see #getIndex()
		 * @see #setPosition(int, int)
		 */
		public PixelBase setIndex(int index);

		/**
		 * Sets the position of this pixel to the specified coordinates.
		 * Positions outside of the underlying images boundaries are not allowed 
		 * but not necessarily enforced either. Accessing image data with a position
		 * outside of the boundaries may cause an Exception.
		 * @param x coordinate (0 left most, width-1 right most)
		 * @param y coordinate (0 top most, height-1 bottom most)
		 * @return this pixel for chaining
		 * 
		 * @see #getX()
		 * @see #getY()
		 * @see #setIndex(int)
		 */
		public PixelBase setPosition(int x, int y);

		/**
		 * Returns the underlying image of this pixel (the image referenced by this pixel)
		 * @return the image this pixel belongs to
		 * 
		 * @see ImgBase
		 */
		public ImgBase<?> getSource();

		/**
		 * Returns a String representation of this pixel at its current position.
		 * The returned String has the following pattern:<br>
		 * <em>Pixelclass</em>[a:<em>alpha</em> r:<em>red</em> g:<em>green</em> b:<em>blue</em>]@(<em>x</em>,<em>y</em>)
		 * @return string representation of this pixel at its current position
		 */
		public default String asString(){
			return String.format("%s[a:%.3f r:%.3f g:%.3f b:%.3f]@(%d,%d)",
					this.getClass().getSimpleName(),
					a_asDouble(),
					r_asDouble(),
					g_asDouble(),
					b_asDouble(),
					getX(),
					getY());
		}

	}
	
	/**
	 * Clamps a value to the range [0,255].
	 * Returns 0 for values less than 0, 255 for values greater than 255, 
	 * and the value it self when in range.
	 * @param val value to be clamped
	 * @return value clamped to [0,255]
	 */
	public final int clamp_0_255(int val){
		return Math.max(0, Math.min(val, 255));
	}

	/**
	 * Clamps a value to the range [0.0, 1.0].
	 * Returns 0.0 for values less than 0, 1.0 for values greater than 1.0, 
	 * and the value it self when in range.
	 * @param val value to be clamped
	 * @return value clamped to [0.0, 1.0]
	 */
	public static double clamp_0_1(double val){
		return Math.max(0.0, Math.min(val, 1.0));
	}

	
	/**
	 * Pixel class for retrieving a value from an {@link Img}.
	 * A Pixel object stores a position and can be used to get and set values of
	 * an Img. It is NOT the value and changing its position will not change the
	 * image, instead it will reference a different value of the image as the
	 * pixel object is a pointer to a value in the Img's data array.
	 * <p>
	 * The Pixel class also provides a set of static methods for color decomposition
	 * and recombination from color channels like {@link #argb(int, int, int, int)}
	 * or {@link #a(int)}, {@link #r(int)}, {@link #g(int)}, {@link #b(int)}.
	 *
	 * @author hageldave
	 * @since 1.0
	 */
	public class Pixel implements PixelBase {
		/** Img this pixel belongs to
		 * @since 1.0 */
		private final Img img;

		/** index of the value this pixel references
		 * @since 1.0 */
		private int index;

		/**
		 * Creates a new Pixel object referencing the value
		 * of specified Img at specified index.
		 * <p>
		 * No bounds checks are performed for index.
		 * @param img the Img this pixel corresponds to
		 * @param index of the value in the images data array
		 * @see #Pixel(Img, int, int)
		 * @see Img#getPixel()
		 * @see Img#getPixel(int, int)
		 * @since 1.0
		 */
		public Pixel(Img img, int index) {
			this.img = img;
			this.index = index;
		}

		/**
		 * Creates a new Pixel object referencing the value
		 * of specified Img at specified position.
		 * <p>
		 * No bounds checks are performed for x and y
		 * @param img the Img this pixel corresponds to
		 * @param x coordinate
		 * @param y coordinate
		 * @see #Pixel(Img, int)
		 * @see Img#getPixel()
		 * @see Img#getPixel(int, int)
		 * @since 1.0
		 */
		public Pixel(Img img, int x, int y) {
			this(img, y*img.getWidth()+x);
		}

		/**
		 * @return the Img this Pixel belongs to.
		 * @since 1.0
		 */
		public Img getSource() {
			return img;
		}

		/**
		 * Sets the index of the Img value this Pixel references.
		 * No bounds checks are performed.
		 * @param index corresponding to the position of the image's data array.
		 * @see #setPosition(int, int)
		 * @see #getIndex()
		 * @since 1.0
		 */
		public Pixel setIndex(int index) {
			this.index = index;
			return this;
		}

		/**
		 * Sets the position of the Img value this Pixel references.
		 * No bounds checks are performed.
		 * @param x coordinate
		 * @param y coordinate
		 * @see #setIndex(int)
		 * @see #getX()
		 * @see #getY()
		 * @since 1.0
		 */
		public Pixel setPosition(int x, int y) {
			this.index = y*img.getWidth()+x;
			return this;
		}

		/**
		 * @return the index of the Img value this Pixel references.
		 * @since 1.0
		 */
		public int getIndex() {
			return index;
		}

		/**
		 * @return the x coordinate of the position in the Img this Pixel references.
		 * @see #getY()
		 * @see #getIndex()
		 * @see #setPosition(int, int)
		 * @since 1.0
		 */
		public int getX() {
			return index % img.getWidth();
		}

		/**
		 * @return the y coordinate of the position in the Img this Pixel references.
		 * @see #getX()
		 * @see #getIndex()
		 * @see #setPosition(int, int)
		 * @since 1.0
		 */
		public int getY() {
			return index / img.getWidth();
		}

		/**
		 * Sets the value of the Img at the position currently referenced by
		 * this Pixel.
		 * <p>
		 * If the position of this pixel is not in bounds of the Img the value for
		 * a different position may be set or an ArrayIndexOutOfBoundsException
		 * may be thrown.
		 * @param pixelValue to be set e.g. 0xff0000ff for blue.
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int, int, int, int)
		 * @see #setRGB(int, int, int)
		 * @see #getValue()
		 * @see Img#setValue(int, int, int)
		 * @since 1.0
		 */
		public Pixel setValue(int pixelValue){
			this.img.getData()[index] = pixelValue;
			return this;
		}

		/**
		 * Gets the value of the Img at the position currently referenced by
		 * this Pixel.
		 * <p>
		 * If the position of this pixel is not in bounds of the Img the value for
		 * a different position may be returned or an ArrayIndexOutOfBoundsException
		 * may be thrown.
		 * @return the value of the Img currently referenced by this Pixel.
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #a()
		 * @see #r()
		 * @see #g()
		 * @see #b()
		 * @see #setValue(int)
		 * @see Img#getValue(int, int)
		 * @since 1.0
		 */
		public int getValue(){
			return this.img.getData()[index];
		}

		/**
		 * @return the alpha component of the value currently referenced by this
		 * Pixel. It is assumed that the value is an ARGB value with 8bits per
		 * color channel, so this will return a value in [0..255].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #r()
		 * @see #g()
		 * @see #b()
		 * @see #setRGB(int, int, int)
		 * @see #setARGB(int, int, int, int)
		 * @see #getValue()
		 * @since 1.0
		 */
		public int a(){
			return a(getValue());
		}

		/**
		 * @return the red component of the value currently referenced by this
		 * Pixel. It is assumed that the value is an ARGB value with 8bits per
		 * color channel, so this will return a value in [0..255].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #a()
		 * @see #g()
		 * @see #b()
		 * @see #setRGB(int, int, int)
		 * @see #setARGB(int, int, int, int)
		 * @see #getValue()
		 * @since 1.0
		 */
		public int r(){
			return r(getValue());
		}

		/**
		 * @return the green component of the value currently referenced by this
		 * Pixel. It is assumed that the value is an ARGB value with 8bits per
		 * color channel, so this will return a value in [0..255].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #a()
		 * @see #r()
		 * @see #b()
		 * @see #setRGB(int, int, int)
		 * @see #setARGB(int, int, int, int)
		 * @see #getValue()
		 * @since 1.0
		 */
		public int g(){
			return g(getValue());
		}

		/**
		 * @return the blue component of the value currently referenced by this
		 * Pixel. It is assumed that the value is an ARGB value with 8bits per
		 * color channel, so this will return a value in [0..255].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #a()
		 * @see #r()
		 * @see #g()
		 * @see #setRGB(int, int, int)
		 * @see #setARGB(int, int, int, int)
		 * @see #getValue()
		 * @since 1.0
		 */
		public int b(){
			return b(getValue());
		}

		/**
		 * @return the normalized alpha component of the value currently referenced by this
		 * Pixel. This will return a value in [0.0 .. 1.0].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #a()
		 * @see #r_asDouble()
		 * @see #g_asDouble()
		 * @see #b_asDouble()
		 * @since 1.2
		 */
		public double a_asDouble(){
			return a_normalized(getValue());
		}

		/**
		 * @return the normalized red component of the value currently referenced by this
		 * Pixel. This will return a value in [0.0 .. 1.0].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #r()
		 * @see #a_asDouble()
		 * @see #g_asDouble()
		 * @see #b_asDouble()
		 * @since 1.2
		 */
		public double r_asDouble(){
			return r_normalized(getValue());
		}

		/**
		 * @return the normalized green component of the value currently referenced by this
		 * Pixel. This will return a value in [0.0 .. 1.0].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #g()
		 * @see #a_asDouble()
		 * @see #r_asDouble()
		 * @see #b_asDouble()
		 * @since 1.2
		 */
		public double g_asDouble(){
			return g_normalized(getValue());
		}

		/**
		 * @return the normalized blue component of the value currently referenced by this
		 * Pixel. This will return a value in [0.0 .. 1.0].
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #b()
		 * @see #a_asDouble()
		 * @see #r_asDouble()
		 * @see #g_asDouble()
		 * @since 1.2
		 */
		public double b_asDouble(){
			return b_normalized(getValue());
		}

		/**
		 * Sets an ARGB value at the position currently referenced by this Pixel.
		 * Each channel value is assumed to be 8bit and otherwise truncated.
		 * @param a alpha
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setRGB(int, int, int)
		 * @see #setRGB_preserveAlpha(int, int, int)
		 * @see #argb(int, int, int, int)
		 * @see #argb_bounded(int, int, int, int)
		 * @see #argb_fast(int, int, int, int)
		 * @see #setValue(int)
		 * @since 1.0
		 */
		public void setARGB(int a, int r, int g, int b){
			setValue(argb(a, r, g, b));
		}

		/**
		 * Sets an opaque RGB value at the position currently referenced by this Pixel.
		 * Each channel value is assumed to be 8bit and otherwise truncated.
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int, int, int, int)
		 * @see #setRGB_preserveAlpha(int, int, int)
		 * @see #argb(int, int, int, int)
		 * @see #argb_bounded(int, int, int, int)
		 * @see #argb_fast(int, int, int, int)
		 * @see #setValue(int)
		 * @since 1.0
		 */
		public void setRGB(int r, int g, int b){
			setValue(rgb(r, g, b));
		}

		/**
		 * Sets an RGB value at the position currently referenced by this Pixel.
		 * The present alpha value will not be altered by this operation.
		 * Each channel value is assumed to be 8bit and otherwise truncated.
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setRGB_fromDouble_preserveAlpha(double, double, double)
		 * @since 1.2
		 */
		public void setRGB_preserveAlpha(int r, int g, int b){
			setValue((getValue() & 0xff000000 ) | argb(0, r, g, b));
		}

		/**
		 * Sets an ARGB value at the position currently referenced by this Pixel. <br>
		 * Each channel value is assumed to be within [0.0 .. 1.0]. Channel values
		 * outside these bounds will be clamped to them.
		 * @param a normalized alpha
		 * @param r normalized red
		 * @param g normalized green
		 * @param b normalized blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #setRGB_fromDouble(double, double, double)
		 * @see #setRGB_fromDouble_preserveAlpha(double, double, double)
		 * @see #setARGB(int, int, int, int)
		 * @see #a_asDouble()
		 * @see #r_asDouble()
		 * @see #g_asDouble()
		 * @see #b_asDouble()
		 * @since 1.2
		 */
		public Pixel setARGB_fromDouble(double a, double r, double g, double b){
			return setValue(argb_fromNormalized(a, r, g, b));
		}

		/**
		 * Sets an opaque RGB value at the position currently referenced by this Pixel. <br>
		 * Each channel value is assumed to be within [0.0 .. 1.0]. Channel values
		 * outside these bounds will be clamped to them.
		 * @param r normalized red
		 * @param g normalized green
		 * @param b normalized blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #setARGB_fromDouble(double, double, double, double)
		 * @see #setRGB_fromDouble_preserveAlpha(double, double, double)
		 * @see #setRGB(int, int, int)
		 * @see #a_asDouble()
		 * @see #r_asDouble()
		 * @see #g_asDouble()
		 * @see #b_asDouble()
		 * @since 1.2
		 */
		public Pixel setRGB_fromDouble(double r, double g, double b){
			return setValue(rgb_fromNormalized(r, g, b));
		}

		/**
		 * Sets an RGB value at the position currently referenced by this Pixel.
		 * The present alpha value will not be altered by this operation. <br>
		 * Each channel value is assumed to be within [0.0 .. 1.0]. Channel values
		 * outside these bounds will be clamped to them.
		 * @param r normalized red
		 * @param g normalized green
		 * @param b normalized blue
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 *
		 * @see #setRGB_preserveAlpha(int, int, int)
		 * @since 1.2
		 */
		public Pixel setRGB_fromDouble_preserveAlpha(double r, double g, double b){
			return setValue((getValue() & 0xff000000) | (0x00ffffff & rgb_fromNormalized(r, g, b)));
		}

		/**
		 * Sets alpha channel value of this Pixel. Value will be truncated to
		 * 8bits (e.g. 0x12ff will truncate to 0xff).
		 * @param a alpha value in range [0..255]
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int a, int r, int g, int b)
		 * @since 1.2
		 */
		public Pixel setA(int a){
			return setValue((getValue() & 0x00ffffff) | ((a<<24) & 0xff000000));
		}

		/**
		 * Sets red channel value of this Pixel. Value will be truncated to
		 * 8bits (e.g. 0x12ff will truncate to 0xff).
		 * @param r red value in range [0..255]
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int a, int r, int g, int b)
		 * @see #setRGB(int r, int g, int b)
		 * @since 1.2
		 */
		public Pixel setR(int r){
			return setValue((getValue() & 0xff00ffff) | ((r<<16) & 0x00ff0000));
		}

		/**
		 * Sets green channel value of this Pixel. Value will be truncated to
		 * 8bits (e.g. 0x12ff will truncate to 0xff).
		 * @param g green value in range [0..255]
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int a, int r, int g, int b)
		 * @see #setRGB(int r, int g, int b)
		 * @since 1.2
		 */
		public Pixel setG(int g){
			return setValue((getValue() & 0xffff00ff) | ((g<<8) & 0x0000ff00));
		}

		/**
		 * Sets blue channel value of this Pixel. Value will be truncated to
		 * 8bits (e.g. 0x12ff will truncate to 0xff).
		 * @param b blue value in range [0..255]
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #setARGB(int a, int r, int g, int b)
		 * @see #setRGB(int r, int g, int b)
		 * @since 1.2
		 */
		public Pixel setB(int b){
			return setValue((getValue() & 0xffffff00) | ((b) & 0x000000ff));
		}

		@Override
		public PixelBase setA_fromDouble(double a) {
			return setA(clamp_0_255((int)Math.round(a*0xff)));
		}

		@Override
		public PixelBase setR_fromDouble(double r) {
			return setR(clamp_0_255((int)Math.round(r*0xff)));
		}

		@Override
		public PixelBase setG_fromDouble(double g) {
			return setG(clamp_0_255((int)Math.round(g*0xff)));
		}

		@Override
		public PixelBase setB_fromDouble(double b) {
			return setB(clamp_0_255((int)Math.round(b*0xff)));
		}

		/**
		 * @return 8bit luminance value of this pixel. <br>
		 * Using weights r=0.2126 g=0.7152 b=0.0722
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #getGrey(int, int, int)
		 * @see #getLuminance(int)
		 * @since 1.2
		 */
		public int getLuminance(){
			return getLuminance(getValue());
		}

		/**
		 * Calculates the grey value of this pixel using specified weights.
		 * @param redWeight weight for red channel
		 * @param greenWeight weight for green channel
		 * @param blueWeight weight for blue channel
		 * @return grey value of pixel for specified weights
		 * @throws ArithmeticException divide by zero if the weights sum up to 0.
		 * @throws ArrayIndexOutOfBoundsException if this Pixel's index is not in
		 * range of the Img's data array.
		 * @see #getLuminance()
		 * @see #getGrey(int, int, int, int)
		 * @since 1.2
		 */
		public int getGrey(final int redWeight, final int greenWeight, final int blueWeight){
			return getGrey(getValue(), redWeight, greenWeight, blueWeight);
		}

		@Override
		public String toString() {
			return asString();
		}


		/* * * * * * * * * */
		// STATIC  METHODS //
		/* * * * * * * * * */

		/**
		 * @param color RGB(24bit) or ARGB(32bit) value
		 * @return 8bit luminance value of given RGB value. <br>
		 * Using weights r=0.2126 g=0.7152 b=0.0722
		 * @see #getGrey(int, int, int, int)
		 * @since 1.0
		 */
		public final int getLuminance(final int color){
			return getGrey(color, 2126, 7152, 722);
		}

		/**
		 * Calculates a grey value from an RGB or ARGB value using specified
		 * weights for each R,G and B channel.
		 * <p>
		 * Weights are integer values so normalized weights need to be converted
		 * beforehand. E.g. normalized weights (0.33, 0.62, 0.05) would be have to
		 * be converted to integer weights (33, 62, 5).
		 * <p>
		 * When using weights with same signs, the value is within [0..255]. When
		 * weights have mixed signs the resulting value is unbounded.
		 * @param color RGB(24bit) or ARGB(32bit) value
		 * @param redWeight weight for red channel
		 * @param greenWeight weight for green channel
		 * @param blueWeight weight for blue channel
		 * @return weighted grey value (8bit) of RGB color value for non-negative weights.
		 * @throws ArithmeticException divide by zero if the weights sum up to 0.
		 * @see #getLuminance(int)
		 * @since 1.0
		 */
		public final int getGrey(final int color, final int redWeight, final int greenWeight, final int blueWeight){
			return (r(color)*redWeight + g(color)*greenWeight + b(color)*blueWeight)/(redWeight+blueWeight+greenWeight);
		}

		/**
		 * Packs 8bit RGB color components into a single 32bit ARGB integer value
		 * with alpha=255 (opaque).
		 * Components are clamped to [0,255].
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @return packed ARGB value
		 *
		 * @see #argb(int, int, int, int)
		 * @see #argb_bounded(int, int, int, int)
		 * @see #argb_fast(int, int, int, int)
		 * @see #rgb(int, int, int)
		 * @see #rgb_fast(int, int, int)
		 * @see #a(int)
		 * @see #r(int)
		 * @see #g(int)
		 * @see #b(int)
		 * @since 1.0
		 */
		public final int rgb_bounded(final int r, final int g, final int b){
			return rgb_fast(
					r > 255 ? 255: r < 0 ? 0:r,
					g > 255 ? 255: g < 0 ? 0:g,
					b > 255 ? 255: b < 0 ? 0:b);
		}

		/**
		 * Packs 8bit RGB color components into a single 32bit ARGB integer value
		 * with alpha=255 (opaque).
		 * Components larger than 8bit get truncated to 8bit.
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @return packed ARGB value
		 *
		 * @see #argb(int, int, int, int)
		 * @see #argb_bounded(int, int, int, int)
		 * @see #argb_fast(int, int, int, int)
		 * @see #rgb_bounded(int, int, int)
		 * @see #rgb_fast(int, int, int)
		 * @see #a(int)
		 * @see #r(int)
		 * @see #g(int)
		 * @see #b(int)
		 * @since 1.0
		 */
		public final int rgb(final int r, final int g, final int b){
			return rgb_fast(r & 0xff, g & 0xff, b & 0xff);
		}

		/**
		 * Packs 8bit RGB color components into a single 32bit ARGB integer value
		 * with alpha=255 (opaque).
		 * Components larger than 8bit are NOT truncated and will result in a
		 * broken, malformed value.
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @return packed ARGB value
		 *
		 * @see #argb(int, int, int, int)
		 * @see #argb_bounded(int, int, int, int)
		 * @see #argb_fast(int, int, int, int)
		 * @see #rgb_bounded(int, int, int)
		 * @see #rgb(int, int, int)
		 * @see #a(int)
		 * @see #r(int)
		 * @see #g(int)
		 * @see #b(int)
		 * @since 1.0
		 */
		public final int rgb_fast(final int r, final int g, final int b){
			return 0xff000000|(r<<16)|(g<<8)|b;
		}

		/**
		 * Packs normalized ARGB color components (values in [0.0 .. 1.0]) into a
		 * single 32bit integer value with alpha=255 (opaque).
		 * Component values less than 0 or greater than 1 clamped to fit the range.
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @return packed ARGB value
		 *
		 * @see #argb_fromNormalized(double, double, double, double)
		 * @see #rgb(int, int, int)
		 * @see #a_normalized(int)
		 * @see #r_normalized(int)
		 * @see #g_normalized(int)
		 * @see #b_normalized(int)
		 * @since 1.2
		 */
		public final int rgb_fromNormalized(final double r, final double g, final double b){
			return rgb_bounded((int)Math.round(r*0xff), (int)Math.round(g*0xff), (int)Math.round(b*0xff));
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
		public final int argb_bounded(final int a, final int r, final int g, final int b){
			return argb_fast(
					a > 255 ? 255: a < 0 ? 0:a,
					r > 255 ? 255: r < 0 ? 0:r,
					g > 255 ? 255: g < 0 ? 0:g,
					b > 255 ? 255: b < 0 ? 0:b);
		}

		/**
		 * Packs 8bit ARGB color components into a single 32bit integer value.
		 * Components larger than 8bit get truncated to 8bit.
		 * @param a alpha
		 * @param r red
		 * @param g green
		 * @param b blue
		 * @return packed ARGB value
		 *
		 * @see #argb_bounded(int, int, int, int)
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
		public final int argb(final int a, final int r, final int g, final int b){
			return argb_fast(a & 0xff, r & 0xff, g & 0xff, b & 0xff);
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
		public final int argb_fast(final int a, final int r, final int g, final int b){
			return (a<<24)|(r<<16)|(g<<8)|b;
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
		public final int argb_fromNormalized(final double a, final double r, final double g, final double b){
			return argb_bounded((int)Math.round(a*0xff), (int)Math.round(r*0xff), (int)Math.round(g*0xff), (int)Math.round(b*0xff));
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
		public final int b(final int color){
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
		public final int g(final int color){
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
		public final int r(final int color){
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
		public final int a(final int color){
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
		public final double b_normalized(final int color){
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
		public final double g_normalized(final int color){
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
		public final double r_normalized(final int color){
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
		public final double a_normalized(final int color){
			return a(color)/255.0;
		}

	}
	
	/**
	 * Class providing convenience methods for converting Images to BufferedImages.
	 * @author hageldave
	 * @since 1.0
	 */
	public static class BufferedImageFactory {

		private BufferedImageFactory(){}
		
		/**
		 * shortcut for get(img, BufferedImage.TYPE_INT_ARGB).
		 * @param img to be copied to BufferedImage of type INT_ARGB
		 * @return a BufferedImage copy of the provided Image
		 * @since 1.0
		 */
		public static BufferedImage getINT_ARGB(Image img){
			return get(img, BufferedImage.TYPE_INT_ARGB);
		}
		
		/**
		 * Creates a new BufferedImage of the specified imgType and same size as 
		 * the provided image and draws the provided Image onto the new BufferedImage.
		 * @param img to be copied to BufferedImage
		 * @param imgType of the BufferedImage. See 
		 * {@link BufferedImage#BufferedImage(int, int, int)} for details on the
		 * available imgTypes.
		 * @return a BufferedImage copy of the provided Image
		 * @since 1.0
		 */
		public static BufferedImage get(Image img, int imgType){
			Function<Integer, ImageObserver> obs = flags->{
				return (image, infoflags, x, y, width, height)->(infoflags & flags)!=flags;
			};
			BufferedImage bimg = new BufferedImage(
					img.getWidth(obs.apply(ImageObserver.WIDTH)), 
					img.getHeight(obs.apply(ImageObserver.HEIGHT)), 
					imgType);
			Graphics2D gr2D = bimg.createGraphics();
			gr2D.drawImage(img, 0, 0, obs.apply(ImageObserver.ALLBITS));
					
			gr2D.dispose();
			
			return bimg;
		}
		
		/**
		 * Instancing method for BufferedImage of type {@link BufferedImage#TYPE_INT_ARGB}
		 * @param d dimension of the created BufferedImage
		 * @return a new BufferedImage of specified dimension and type TYPE_INT_ARGB
		 * @since 1.0
		 */
		public static BufferedImage getINT_ARGB(Dimension d){
			return getINT_ARGB(d.width, d.height);
		}
		
		/**
		 * Instancing method for BufferedImage of type {@link BufferedImage#TYPE_INT_ARGB}
		 * @param width of the created BufferedImage
		 * @param height of the created BufferedImage
		 * @return a new BufferedImage of specified dimension and type TYPE_INT_ARGB
		 * @since 1.0
		 */
		public static BufferedImage getINT_ARGB(int width, int height){
			return new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
		}
	}
	
	/**
	 * Class holding all of the {@link Iterator} and {@link Spliterator} classes
	 * used in the {@link ImgBase} interface.
	 * <p>
	 * <b>NOTE ON ELEMENTS OF ITERATORS/SPLITERATORS</b><br>
	 * <i>All of the iterators/spliterators in this class implement the following paradigm:</i><br>
	 * As image implementations typically use buffers or arrays of a native datatype as a
	 * representation of their pixel values, they are not collections of a pixel datatype.
	 * The pixel datatypes used here are thus pointers into an image's array data structure.
	 * <u>When iterating the pixel data of an image, a pixel object will be reused for each
	 * image pixel.</u> This is done to avoid excessive allocation of pixel objects and keep
	 * the garbage collector 'asleep' (as GC heavily impacts performance). This means that
	 * the elements (pixel objects) returned by an iterator/spliterator are not distinct.
	 * 
	 * @author hageldave
	 * @since 2.0
	 */
	public static final class Iterators {

		private Iterators(){/*not to be instantiated*/}


		/**
		 * The standard {@link Iterator} class for images.
		 * @author hageldave
		 * @param <P> the pixel type of this iterator
		 */
		public static class ImgIterator<P extends PixelBase> implements Iterator<P> {
			
			private final P px;
			private final int numValues;
			private int index;

			/**
			 * Creates a new ImgIterator over the image of the specified pixel.
			 * The specified pixel will be reused on every invocation of {@link #next()}
			 * with incremented index.
			 * 
			 * @param px pixel that will be used to iterate its image
			 */
			public ImgIterator(P px) {
				this.px = px;
				this.numValues = px.getSource().numValues();
				this.index = -1;
			}

			@Override
			public P next() {
				px.setIndex(++index);
				return px;
			}

			@Override
			public boolean hasNext() {
				return index+1 < numValues;
			}

			@Override
			public void forEachRemaining(Consumer<? super P> action) {
				px.setIndex(++index);
				for(/*index*/; index < numValues; px.setIndex(++index)){
					action.accept(px);
				}
			}
		}


		/**
		 * The standard iterator class for iterating over an area of an image.
		 * @author hageldave
		 * @param <P> the pixel type of this iterator
		 */
		public static class ImgAreaIterator<P extends PixelBase> implements Iterator<P> {
			private final P px;
			private final int xStart;
			private final int yStart;
			private final int width;
			private final int height;
			private int x;
			private int y;

			/**
			 * Creates a new ImgAreaIterator for iterating the pixels in the specified area of an image.
			 * The iterated image is the source of the specified pixel.
			 * The specified pixel will be reused on every invocation of {@link #next()}
			 * with incremented index.
			 * 
			 * @param xStart the left boundary of the area (inclusive)
			 * @param yStart the top boundary of the area (inclusive)
			 * @param width of the area
			 * @param height of the area
			 * @param px the pixel used for iterating
			 */
			public ImgAreaIterator(final int xStart, final int yStart, final int width, final int height, P px) {
				this.px = px;
				this.xStart = xStart;
				this.yStart = yStart;
				this.width = width;
				this.height = height;
				this.x=0;
				this.y=0;
			}


			@Override
			public P next() {
				px.setPosition(x+xStart, y+yStart);
				x++;
				if(x >= width){
					x=0;
					y++;
				}
				return px;
			}

			@Override
			public boolean hasNext() {
				return y < height;
			}

			@Override
			public void forEachRemaining(Consumer<? super P> action) {
				int xEnd = xStart+width;
				int yEnd = yStart+height;
				int x = this.x+xStart;
				for(int y=this.y+yStart; y < yEnd; y++){
					for(; x < xEnd; x++){
						px.setPosition(x, y);
						action.accept(px);
					}
					x = xStart;
				}
			}
		}
		
		/**
		 * The standard {@link Spliterator} class for images.
		 * @author hageldave
		 */
		public static final class ImgSpliterator<P extends PixelBase> implements Spliterator<P> {
		
			private final Supplier<P> pixelSupplier;
			private final P px;
			private int endIndex;
			private final int minimumSplitSize;
		
			/**
			 * Constructs a new ImgSpliterator for the specified index range
			 * @param startIndex first index of the range (inclusive)
			 * @param endIndex last index of the range (inclusive)
			 * @param minSplitSize minimum split size for this spliterator (minimum number of elements in a split)
			 * @param pixelSupplier a function that allocates a new pixel
			 * that points to the index given by the function argument
			 * @since 1.0
			 */
			public ImgSpliterator(int startIndex, int endIndex, int minSplitSize, Supplier<P> pixelSupplier) {
				this.pixelSupplier = pixelSupplier;
				this.px = pixelSupplier.get();
				this.px.setIndex(startIndex);
				this.endIndex = endIndex;
				this.minimumSplitSize = minSplitSize;
			}
		
			private void setEndIndex(int endIndex) {
				this.endIndex = endIndex;
			}
		
			@Override
			public boolean tryAdvance(final Consumer<? super P> action) {
				if(px.getIndex() <= endIndex){
					int index = px.getIndex();
					action.accept(px);
					px.setIndex(index+1);
					return true;
				} else {
					return false;
				}
			}
		
			@Override
			public void forEachRemaining(final Consumer<? super P> action) {
				int idx = px.getIndex();
				for(;idx <= endIndex; px.setIndex(++idx)){
					action.accept(px);
				}
			}
		
			@Override
			public Spliterator<P> trySplit() {
				int currentIdx = Math.min(px.getIndex(), endIndex);
				int range = endIndex+1-currentIdx;
				if(range/2 >= minimumSplitSize){
					int mid = currentIdx+range/2;
					ImgSpliterator<P> split = new ImgSpliterator<>(mid, endIndex, minimumSplitSize, pixelSupplier);
					setEndIndex(mid-1);
					return split;
				} else {
					return null;
				}
			}
		
			@Override
			public long estimateSize() {
				int currentIndex = px.getIndex();
				int lastIndexPlusOne = endIndex+1;
				return lastIndexPlusOne-currentIndex;
			}
		
			@Override
			public int characteristics() {
				return NONNULL | SIZED | CONCURRENT | SUBSIZED | IMMUTABLE;
			}
		
		}


		/**
		 * Spliterator class for images bound to a specific area
		 * @author hageldave
		 */
		public static final class ImgAreaSpliterator<P extends PixelBase> implements Spliterator<P> {

			private final Supplier<P> pixelSupplier;
			private final P px;
			/* start x coord and end x coord of a row */
			private final int startX, endXexcl;
			/* current coords of this spliterator */
			private int x,y;
			/* final coords of this spliterator */
			private int finalXexcl, finalYincl;

			private final int minimumSplitSize;

			/**
			 * Constructs a new ImgAreaSpliterator for the specified area
			 * @param xStart left boundary of the area (inclusive)
			 * @param yStart upper boundary of the area (inclusive)
			 * @param width of the area
			 * @param height of the area
			 * @param minSplitSize the minimum number of elements in a split
			 * @param pixelSupplier a function that allocates a new pixel
			 * @since 1.1
			 */
			public ImgAreaSpliterator(
					int xStart,
					int yStart,
					int width,
					int height,
					int minSplitSize,
					Supplier<P> pixelSupplier
			){
				this(xStart, xStart+width, xStart, yStart, xStart+width, yStart+height-1, minSplitSize, pixelSupplier);
			}

			private ImgAreaSpliterator(
					int xStart,
					int endXexcl,
					int x,
					int y,
					int finalXexcl,
					int finalYincl,
					int minSplitSize,
					Supplier<P> pixelSupplier
			){
				this.startX = xStart;
				this.endXexcl = endXexcl;
				this.x = x;
				this.y = y;
				this.finalXexcl = finalXexcl;
				this.finalYincl = finalYincl;
				this.pixelSupplier = pixelSupplier;
				this.px = pixelSupplier.get();
				this.px.setPosition(x, y);
				this.minimumSplitSize = minSplitSize;
			}


			@Override
			public boolean tryAdvance(final Consumer<? super P> action) {
				if(y > finalYincl || (y == finalYincl && x >= finalXexcl)){
					return false;
				} else {
					action.accept(px);
					if(x+1 >= endXexcl){
						x = startX;
						y++;
					} else {
						x++;
					}
					px.setPosition(x, y);
					return true;
				}
			}

			@Override
			public void forEachRemaining(final Consumer<? super P> action) {
				if(this.y == finalYincl){
					for(int x = this.x; x < finalXexcl; x++){
						px.setPosition(x, finalYincl);
						action.accept(px);
					}
				} else {
					// end current row
					for(int x = this.x; x < endXexcl; x++){
						px.setPosition(x, this.y);
						action.accept(px);
					}
					// do next rows right before final row
					for(int y = this.y+1; y < this.finalYincl; y++){
						for(int x = startX; x < endXexcl; x++ ){
							px.setPosition(x, y);
							action.accept(px);
						}
					}
					// do final row
					for(int x = startX; x < finalXexcl; x++){
						px.setPosition(x, finalYincl);
						action.accept(px);
					}
				}
			}

			@Override
			public Spliterator<P> trySplit() {
				int width = (this.endXexcl-this.startX);
				int idx = this.x - this.startX;
				int finalIdx_excl = (this.finalYincl-this.y)*width + (this.finalXexcl-startX);
				int midIdx_excl = idx + (finalIdx_excl-idx)/2;
				if(midIdx_excl > idx+minimumSplitSize){
					int newFinalX_excl = startX + (midIdx_excl%width);
					int newFinalY_incl = this.y + midIdx_excl/width;
					ImgAreaSpliterator<P> split = new ImgAreaSpliterator<>(
							startX,         // start of a row
							endXexcl,       // end of a row
							newFinalX_excl, // x coord of new spliterator
							newFinalY_incl, // y coord of new spliterator
							finalXexcl,     // final x coord of new spliterator
							finalYincl,    // final y coord of new spliterator
							minimumSplitSize,
							pixelSupplier);

					// shorten this spliterator because new one takes care of the rear part
					this.finalXexcl = newFinalX_excl;
					this.finalYincl = newFinalY_incl;

					return split;
				} else {
					return null;
				}
			}

			@Override
			public long estimateSize() {
				int idx = this.x - this.startX;
				int finalIdx_excl = (this.finalYincl-this.y)*(this.endXexcl-this.startX) + (this.finalXexcl-startX);
				return finalIdx_excl-idx;
			}

			@Override
			public int characteristics() {
				return NONNULL | SIZED | CONCURRENT | SUBSIZED | IMMUTABLE;
			}

		}
		/**
		 * Special Spliterator for images which guarantees that each split will cover at least
		 * an entire row of the image.
		 * @author hageldave
		 */
		public static final class RowSpliterator<P extends PixelBase> implements Spliterator<P> {

			private final int startX;
			private final int endXinclusive;
			private int x;
			private int y;
			private int endYinclusive;
			private final Supplier<P> pixelSupplier;
			private final P px;

			/**
			 * Creates a new RowSpliterator for iterating the pixels in the specified area.
			 * Each split is guaranteed to cover at least 1 entire row of the area.
			 * @param startX left boundary of the area (inclusive)
			 * @param width width of the area
			 * @param startY top boundary of the area (inclusive)
			 * @param endYincl bottom boundary of the area (inclusive)
			 * @param pixelSupplier a function that allocates a new pixel
			 */
			public RowSpliterator(int startX, int width, int startY, int endYincl, Supplier<P> pixelSupplier) {
				this.startX = startX;
				this.x = startX;
				this.endXinclusive = startX+width-1;
				this.y = startY;
				this.endYinclusive = endYincl;
				this.pixelSupplier = pixelSupplier;
				this.px = pixelSupplier.get();
				this.px.setPosition(x,y);
			}


			@Override
			public boolean tryAdvance(Consumer<? super P> action) {
				if(x <= endXinclusive){
					px.setPosition(x, y);
					x++;
				} else if(y < endYinclusive) {
					y++;
					x=startX;
					px.setPosition(x, y);
					x++;
				} else {
					return false;
				}
				action.accept(px);
				return true;
			}

			@Override
			public void forEachRemaining(Consumer<? super P> action) {
				int x_ = x;
				for(int y_ = y; y_ <= endYinclusive; y_++){
					for(;x_ <= endXinclusive; x_++){
						px.setPosition(x_, y_);
						action.accept(px);
					}
					x_=startX;
				}
			}

			@Override
			public Spliterator<P> trySplit() {
				if(this.y < endYinclusive){
					int newY = y + 1 + (endYinclusive-y)/2;
					RowSpliterator<P> split = new RowSpliterator<>(startX, endXinclusive-startX+1, newY, endYinclusive, pixelSupplier);
					this.endYinclusive = newY-1;
					return split;
				} else return null;
			}

			@Override
			public long estimateSize() {
				return (endYinclusive-y)*(endXinclusive+1-startX)+endXinclusive+1-x;
			}

			@Override
			public int characteristics() {
				return NONNULL | SIZED | CONCURRENT | SUBSIZED | IMMUTABLE;
			}

		}
		/**
		 * Special Spliterator which guarantees that each split will cover at least
		 * an entire column of the image.
		 * @author hageldave
		 */
		public static final class ColSpliterator<P extends PixelBase> implements Spliterator<P> {

			private final int startY;
			private int endXinclusive;
			private int x;
			private int y;
			private final int endYinclusive;
			private final Supplier<P> pixelSupplier;
			private final P px;

			/**
			 * Creates a new RowSpliterator for iterating the pixels in the specified area.
			 * Each split is guaranteed to cover at least 1 entire row of the area.
			 * @param startX left boundary of the area (inclusive)
			 * @param endXincl right boundary of the area (inclusive)
			 * @param startY top boundary of the area (inclusive)
			 * @param height of the area
			 * @param pixelSupplier a function that allocates a new pixel
			 */
			public ColSpliterator(int startX, int endXincl, int startY, int height, Supplier<P> pixelSupplier) {
				this.startY = startY;
				this.y = startY;
				this.endYinclusive = startY+height-1;
				this.x = startX;
				this.endXinclusive = endXincl;
				this.pixelSupplier = pixelSupplier;
				this.px = pixelSupplier.get();
				this.px.setPosition(x, y);
			}


			@Override
			public boolean tryAdvance(Consumer<? super P> action) {
				if(y <= endYinclusive){
					px.setPosition(x, y);
					y++;
				} else if(x < endXinclusive) {
					x++;
					y=startY;
					px.setPosition(x, y);
					y++;
				} else {
					return false;
				}
				action.accept(px);
				return true;
			}

			@Override
			public void forEachRemaining(Consumer<? super P> action) {
				int y_ = y;
				for(int x_ = x; x_ <= endXinclusive; x_++){
					for(;y_ <= endYinclusive; y_++){
						px.setPosition(x_, y_);
						action.accept(px);
					}
					y_=startY;
				}
			}

			@Override
			public Spliterator<P> trySplit() {
				if(this.x < endXinclusive){
					int newX = x + 1 + (endXinclusive-x)/2;
					ColSpliterator<P> split = new ColSpliterator<>(newX, endXinclusive, startY, endYinclusive-startY+1, pixelSupplier);
					this.endXinclusive = newX-1;
					return split;
				} else return null;
			}

			@Override
			public long estimateSize() {
				return (endXinclusive-x)*(endYinclusive+1-startY)+endYinclusive+1-y;
			}

			@Override
			public int characteristics() {
				return NONNULL | SIZED | CONCURRENT | SUBSIZED | IMMUTABLE;
			}

		}

	}


	/**
	 * Throws an {@link IllegalArgumentException} when the specified area 
	 * is not within the bounds of the specified image, or if the area
	 * is not positive.
	 * This is used for parameter evaluation.
	 * 
	 * @param xStart left boundary of the area
	 * @param yStart top boundary of the area
	 * @param width of the area
	 * @param height of the area
	 * @param img the area has to fit in.
	 * @throws IllegalArgumentException when area not in image bounds or not area not positive
	 */
	public static void requireAreaInImageBounds(final int xStart, final int yStart, final int width, final int height, ImgBase<?> img){
		if(		width <= 0 || height <= 0 ||
				xStart < 0 || yStart < 0 ||
				xStart+width > img.getWidth() || yStart+height > img.getHeight() )
		{
			throw new IllegalArgumentException(String.format(
							"provided area [%d,%d][%d,%d] is not within bounds of the image [%d,%d]",
							xStart,yStart,width,height, img.getWidth(), img.getHeight()));
		}
	}
	
	
	
	/**
	 * The PixelConvertingSpliterator enables iterating an {@link Img} with a
	 * different datatype than {@link Pixel} ({@link Img#spliterator()}). This
	 * can come in handy if a {@link Consumer} would be easier to write when not
	 * restricted to the Pixel datatype.
	 * <p>
	 * For example, a specific operation should be applied to each pixel of the Img
	 * which is specified as a function that takes a 3-dimensional vector as
	 * argument with red, green and blue component e.g.
	 * <br><tt>applyOperation(Vector3D vec){...}}</tt><br>
	 * To obtain a spliterator of the Img that will accept a {@code Consumer<Vector3D>}
	 * a PixelConvertingSpliterator can be created from an ordinary {@code Spliterator<Pixel>}
	 * providing the following functions:<br>
	 * <ul><li>
	 * <u>element allocator:</u> a function to create (allocate) an object of the
	 * desired datatype (Vector3D in this example)
	 * </li><li>
	 * <u>from Pixel converter:</u> a function to convert a Pixel object to an object
	 * of the desired datatype e.g. <br>{@code void convertPixel(Pixel px, Vector3D vec)}
	 * </li><li>
	 * <u>to Pixel converter:</u> a function to convert an object of the desired
	 * datatype to a Pixel object e.g. <br>{@code void convertVector(Vector3D vec, Pixel px)}
	 * </li></ul><br>
	 * A {@code PixelConvertingSpliterator<Vector3D>} would then be created like this:
	 * <pre>
	 * {@code
	 * Img img=loadImgFromSomewhere();
	 * Spliterator<Vector3D> split = new PixelConvertingSpliterator<>(
	 *       img.spliterator(),
	 *       ()->{return new Vector3D();},
	 *       MyConverter::convertPixel,
	 *       MyConverter::convertVector
	 *       );
	 * StreamSupport.stream(split, true)
	 *       .forEach(VectorOperations::applyOperation);
	 * }</pre>
	 * <p>
	 * The reason for not simply doing an 'on the fly' conversion inside a {@code Spliterator<Pixel>}
	 * or using the {@link Stream#map(java.util.function.Function)} function is,
	 * that these methods are prone to excessive object allocation (allocating a
	 * new object for every pixel). When using the PixelConvertingSpliterator there
	 * is only one object allocation per split, and the object will be reused for
	 * each pixel within that split.
	 *
	 * @author hageldave
	 * @param <P> the pixel type of the underlying Spliterator
	 * @param <T> the type of elements returned by the PixelConvertingSpliterator
	 * @since 1.4
	 */
	public static class PixelConvertingSpliterator<P extends PixelBase, T> implements Spliterator<T> {

		/** {@code Spliterator<Pixel>} acting as delegate of this spliterator
		 * @since 1.4 */
		protected final Spliterator<? extends P> delegate;

		/** the element of this spliterator (reused on each pixel of the delegate)
		 * @since 1.4 */
		protected final T element;


		protected PixelConverter<P,T> converter;


		/**
		 * Constructs a new PixelConvertingSpliterator.
		 *
		 * @param delegate the {@code Spliterator<Pixel>} this spliterator delegates to.
		 * @param elementAllocator method for allocating an object of this spliterator's
		 * element type.
		 * @param fromPixelConverter method for setting up an element of this spliterator
		 * according to its underlying pixel.
		 * @param toPixelConverter method for adopting an underlying pixel value
		 * according to an element of this spliterator.
		 *
		 * @since 1.4
		 */
		public PixelConvertingSpliterator(
				Spliterator<? extends P> delegate,
				Supplier<T> elementAllocator,
				BiConsumer<P, T> fromPixelConverter,
				BiConsumer<T, P> toPixelConverter)
		{
			this(delegate, PixelConverter.fromFunctions(
					elementAllocator,
					fromPixelConverter,
					toPixelConverter)
				);
		}

		public PixelConvertingSpliterator(Spliterator<? extends P> delegate, PixelConverter<P,T> converter) {
			this.converter = converter;
			this.delegate=delegate;
			this.element = converter.allocateElement();
		}

		@Override
		public boolean tryAdvance(Consumer<? super T> action) {
			return delegate.tryAdvance( px -> {
				converter.convertPixelToElement(px, element);
				action.accept(element);
				converter.convertElementToPixel(element, px);
			});
		}

		@Override
		public void forEachRemaining(Consumer<? super T> action) {
			delegate.forEachRemaining(px -> {
				converter.convertPixelToElement(px, element);
				action.accept(element);
				converter.convertElementToPixel(element, px);
			});
		}

		@Override
		public Spliterator<T> trySplit() {
			Spliterator<? extends P> del = delegate.trySplit();
			return del == null ? null:new PixelConvertingSpliterator<P,T>(del,converter);
		}

		@Override
		public long estimateSize() {
			return delegate.estimateSize();
		}

		@Override
		public int characteristics() {
			return delegate.characteristics();
		}

		/**
		 * Example implementation of a {@code PixelConvertingSpliterator<double[]>}.
		 * <p>
		 * The elements of the returned spliterator will be {@code double[]} of length 3
		 * with normalized red, green and blue channels on index 0, 1 and 2.
		 * <p>
		 * <b>Code:</b>
		 * <pre>
		 * {@code
		 * Supplier<double[]> arrayAllocator = () -> {
		 *    return new double[3];
		 * };
		 * BiConsumer<Pixel, double[]> convertToArray = (px, array) -> {
		 *    array[0]=px.r_normalized();
		 *    array[1]=px.g_normalized();
		 *    array[2]=px.b_normalized();
		 * };
		 * BiConsumer<double[], Pixel> convertToPixel = (array, px) -> {
		 *    px.setRGB_fromNormalized_preserveAlpha(
		 *       // clamp values between zero and one
		 *       Math.min(1, Math.max(0, array[0])),
		 *       Math.min(1, Math.max(0, array[1])),
		 *       Math.min(1, Math.max(0, array[2])));
		 * };
		 * PixelConvertingSpliterator<double[]> arraySpliterator = new PixelConvertingSpliterator<>(
		 *    pixelSpliterator,
		 *    arrayAllocator,
		 *    convertToArray,
		 *    convertToPixel);
		 * }
		 * </pre>
		 * @param pixelSpliterator the {@code Spliterator<Pixel>} to which the
		 * returned spliterator delegates to.
		 * @return a spliterator with float[] elements consisting of normalized RGB channels.
		 *
		 * @since 1.4
		 */
		public static PixelConvertingSpliterator<PixelBase, double[]> getDoubletArrayElementSpliterator(
				Spliterator<? extends PixelBase> pixelSpliterator){
			PixelConvertingSpliterator<PixelBase, double[]> arraySpliterator = new PixelConvertingSpliterator<>(
					pixelSpliterator, getDoubleArrayConverter());
			return arraySpliterator;
		}

		/**
		 * @return Exemplary PixelConverter that converts to double[].
		 */
		public static PixelConverter<PixelBase, double[]> getDoubleArrayConverter(){
			return new PixelConverter<PixelBase, double[]>() {

				@Override
				public void convertPixelToElement(PixelBase px, double[] array) {
					array[0]=px.r_asDouble();
					array[1]=px.g_asDouble();
					array[2]=px.b_asDouble();
				}

				@Override
				public void convertElementToPixel(double[] array, PixelBase px) {
					px.setRGB_fromDouble_preserveAlpha(
							// clamp values between zero and one
							clamp_0_1(array[0]),
							clamp_0_1(array[1]),
							clamp_0_1(array[2]));
				}

				@Override
				public double[] allocateElement() {
					return new double[3];
				}
			};
		}



		public static interface PixelConverter<P extends PixelBase, T> {
			/**
			 * Allocates a new element for the PixelConvertingSpliterator
			 * (will be called once per split)
			 * @return element, probably in uninitialized state
			 */
			public T allocateElement();
			
			/**
			 * converts the specified pixel to the specified element
			 * (initiliazation of previously allocated element).
			 * <br><b>NOT ALLOCATION, ONLY SETUP OF THE ELEMENT</b>
			 * @param px to used for setting up the element
			 * @param element to be set up
			 */
			public void convertPixelToElement(P px, T element);
			
			/**
			 * converts the specified element back to the specified pixel
			 * (set pixel value according to element).
			 * <br><b>NOT ALLOCATION, ONLY SETTING THE PIXEL VALUE ACCORDINGLY</b>
			 * @param element to be used for setting the pixel value
			 * @param px to be set
			 */
			public void convertElementToPixel(T element, P px);

			/**
			 * Creates a new PixelConverter from the specified functions.
			 * @param allocator a supplier that allocates a new object of the element type (see {@link #allocateElement()})
			 * @param pixelToElement a consumer that sets the contents of the element according to a pixel (see {@link #convertPixelToElement(PixelBase, Object)})
			 * @param elementToPixel a consumer that sets the content of a pixel according to the element (see {@link #convertElementToPixel(Object, PixelBase)})
			 * @return a new PixelConverter
			 */
			public static <P extends PixelBase, T> PixelConverter<P,T> fromFunctions(
					Supplier<T> allocator,
					BiConsumer<P,T> pixelToElement,
					BiConsumer<T,P> elementToPixel)
			{
				Objects.requireNonNull(allocator);
				BiConsumer<P,T> px_2_el = pixelToElement==null ? (px,e)->{}:pixelToElement;
				BiConsumer<T,P> el_2_px = elementToPixel==null ? (e,px)->{}:elementToPixel;

				return new PixelConverter<P,T>(){
					@Override
					public T allocateElement() {return allocator.get();}
					@Override
					public void convertPixelToElement(P px, T element) {px_2_el.accept(px, element);}
					@Override
					public void convertElementToPixel(T element, P px) {el_2_px.accept(element, px);}

				};
			}
		}

	}

	
	


	/**
	 * The PixelManipulator interface defines an action to be performed on a pixel.
	 * The action however is performed on a different representation of the pixel which
	 * is given by the manipulators converter.
	 * <p>
	 * This works the following way:<br>
	 * <ol>
	 * <li>convert pixel to element using the converter</li>
	 * <li>apply the action on the element</li>
	 * <li>convert the element back to pixel using the converter</li>
	 * </ol>
	 * <p>
	 * This is used with an image's <tt>forEach</tt> method (see {@link ImgBase#forEach(PixelManipulator)})
	 * 
	 * @author hageldave
	 * @param <P> the pixel type of the converter (PixelBase or an implementation of it)
	 * @param <T> the element type of the converter (the type a pixel is converted to before applying the action)
	 * @since 2.0
	 */
	public interface PixelManipulator<P extends PixelBase,T> {

		/**
		 * Returns the converter that converts a pixel to the element 
		 * accepted by this manipulators action (and the element back to the pixel).
		 * @return this manipulator's converter
		 */
		public PixelConverter<P, T> getConverter();

		/**
		 * Returns the action performed by this manipulator
		 * @return this manipulator's action
		 */
		public Consumer<T> getAction();

		/**
		 * Creates a PixelManipulator from a {@link PixelConverter} and corresponding {@link Consumer} (action).
		 * @param converter of the PixelManipulator
		 * @param action of the Manipulator
		 * @return a PixelManipulator consisting of the specified converter and action.
		 */
		public static <P extends PixelBase,T> PixelManipulator<P,T> fromConverterAndConsumer(PixelConverter<P, T> converter, Consumer<T> action){
			return new PixelManipulator<P,T>() {
				@Override
				public PixelConverter<P, T> getConverter() {
					return converter;
				}
				@Override
				public Consumer<T> getAction() {
					return action;
				}
			};
		}


	}
	
	/**
	 * CountedCompleter class for multithreaded execution of a Consumer on a
	 * Spliterator. Used to realise multithreaded forEach loop in {@link ImgBase#forEach(Consumer)}.
	 * 
	 * @author hageldave
	 * @see ImgBase#forEach(boolean parallel, Consumer action)
	 * @since 2.0 (relocated from Img class)
	 */
	public final class ParallelForEachExecutor<T> extends CountedCompleter<Void> {
		private static final long serialVersionUID = 1L;

		private final Spliterator<T> spliterator;
		private final Consumer<? super T> action;
		
		/**
		 * Creates a new ParallelForEachExecutor that executes the 
		 * specified {@link Consumer} (action) on the elements of the specified {@link Spliterator}.
		 * In parallel.
		 * <p>
		 * Call {@link #invoke()} to trigger execution.
		 * 
		 * @param spliterator that provides the elements on which the action is to be performed
		 * @param action to be performed
		 */
		public ParallelForEachExecutor(
				Spliterator<T> spliterator,
				Consumer<? super T> action)
		{
			this(null, spliterator, action);
		}

		private ParallelForEachExecutor(
				ParallelForEachExecutor<T> parent,
				Spliterator<T> spliterator,
				Consumer<? super T> action)
		{
			super(parent);
			this.spliterator = spliterator;
			this.action = action;
		}

		@Override
		public void compute() {
			Spliterator<T> sub;
			while ((sub = spliterator.trySplit()) != null) {
				addToPendingCount(1);
				new ParallelForEachExecutor<T>(this, sub, action).fork();
			}
			spliterator.forEachRemaining(action);
			propagateCompletion();
		}
	}

	
	/**
	 * Base interface for imagingkit's Img classes.
	 * <p>
	 * This interface defines the most basic methods like getting the dimensions
	 * of an image and converting an image to {@link BufferedImage}.
	 * <p>
	 * Appart from that it defines and implements all the {@link Iterable} functionality
	 * which is based on {@link PixelBase}. The Iterable Functionality also comprises
	 * {@link Spliterator}s as well as the {@link #forEach(Consumer)} and {@link #stream()}
	 * functionality.
	 * <p>
	 * The {@link Graphics2D} related functionality like {@link #createGraphics()}
	 * and {@link #paint(Consumer)} is by default based on {@link #getRemoteBufferedImage()}.
	 * If it is possible to create a remote BufferedImage from the implemented
	 * data structure, the method should be overridden to enable the mentioned funtionality.
	 *
	 * @param <P> the pixel type of the image
	 *
	 * @author hageldave
	 * @since 2.0
	 */
	public interface ImgBase<P extends PixelBase> extends Iterable<P> {

		/**
		 * @return the dimension (width,height) of this image
		 * 
		 * @see #getWidth()
		 * @see #getHeight()
		 * @see #numValues()
		 */
		public default Dimension getDimension(){ return new Dimension(getWidth(),getHeight());}

		/**
		 * @return the width of this image (number of pixels in horizontal direction)
		 * 
		 * @see #getHeight()
		 * @see #getDimension()
		 * @see #numValues()
		 */
		public int getWidth();

		/**
		 * @return the height of this image (number of pixels in vertical direction)
		 * 
		 * @see #getWidth()
		 * @see #getDimension()
		 * @see #numValues()
		 */
		public int getHeight();

		/**
		 * @return the number of pixels of this image
		 * 
		 * @see #getWidth()
		 * @see #getHeight()
		 * @see #getDimension()
		 */
		public default int numValues(){return getWidth()*getHeight();}

		/**
		 * Creates a new pixel object (instance of {@link PixelBase}) for this Img 
		 * with initial position (0,0) i.e. top left corner.
		 * <p>
		 * <b>Tip:</b><br>
		 * Do not use this method repeatedly while iterating the image.
		 * Use {@link PixelBase#setPosition(int, int)} instead to avoid excessive
		 * allocation of pixel objects.
		 * <br>
		 * You can also use <code>for(PixelBase px: img){...}</code> syntax or the
		 * {@link #forEach(Consumer)} method to iterate this image.
		 * 
		 * @return a pixel object for this image.
		 * 
		 * @see #getPixel(int, int)
		 */
		public P getPixel();

		/**
		 * Creates a new Pixel object for this Img at specified position.
		 * (0,0) is the top left corner, (width-1,height-1) is the bottom right corner.
		 * <p>
		 * <b>Tip:</b><br>
		 * Do not use this method repeatedly while iterating the image.
		 * Use {@link PixelBase#setPosition(int, int)} instead to avoid excessive
		 * allocation of pixel objects.
		 * <br>
		 * You can also use <code>for(PixelBase px: img){...}</code> syntax or the
		 * {@link #forEach(Consumer)} method to iterate this image.
		 * 
		 * @param x coordinate
		 * @param y coordinate
		 * @return a Pixel object for this Img at {x,y}.
		 * 
		 * @see #getPixel()
		 */
		public P getPixel(int x, int y);

		/**
		 * Copies this image's data to the specified {@link BufferedImage}.
		 * This method will preserve the {@link Raster} of the specified
		 * BufferedImage and will only modify the contents of it.
		 * 
		 * @param bimg the BufferedImage
		 * @return the specified BufferedImage
		 * @throws IllegalArgumentException if the provided BufferedImage
		 * has a different dimension as this image.
		 * 
		 * @see #toBufferedImage()
		 * @see #getRemoteBufferedImage()
		 */
		public BufferedImage toBufferedImage(BufferedImage bimg);

		/**
		 * @return a BufferedImage of type INT_ARGB with this Img's data copied to it.
		 * 
		 * @see #toBufferedImage(BufferedImage)
		 * @see #getRemoteBufferedImage()
		 */
		public default BufferedImage toBufferedImage(){
			BufferedImage bimg = BufferedImageFactory.getINT_ARGB(getDimension());
			return toBufferedImage(bimg);
		}

		/**
		 * Creates a {@link BufferedImage} that shares the data of this image. Changes in
		 * this image are reflected in the created BufferedImage and vice versa.
		 * The {@link ColorModel} and {@link Raster} of the resulting BufferedImage
		 * are implementation dependent.
		 * <p>
		 * This operation may not be supported by an implementation of {@link ImgBase}
		 * and will then throw an {@link UnsupportedOperationException}. Use
		 * {@link #supportsRemoteBufferedImage()} to check if this operation is
		 * supported.
		 * 
		 * @return BufferedImage sharing this Img's data.
		 * @throws UnsupportedOperationException if this implementation of {@link ImgBase}
		 * does not support this method.
		 * 
		 * @see #supportsRemoteBufferedImage()
		 * @see #toBufferedImage(BufferedImage)
		 * @see #toBufferedImage()
		 */
		public default BufferedImage getRemoteBufferedImage(){
			throw new UnsupportedOperationException("This method is not supported. You can check beforehand using supportsRemoteBufferedImage()");
		}


		/**
		 * Returns true when this implementation of {@link ImgBase} supports the 
		 * {@link #getRemoteBufferedImage()} method. This by default also indicates
		 * the support for the following methods:
		 * <ul>
		 * <li>{@link #createGraphics()}</li>
		 * <li>{@link #paint(Consumer)}</li>
		 * </ul>
		 * 
		 * @return true when supported, false otherwise.
		 */
		public default boolean supportsRemoteBufferedImage(){
			return false;
		}

		/**
		 * Creates a {@link Graphics2D}, which can be used to draw into this image.
		 * <br>
		 * This operation may not be supported by an implementation of {@link ImgBase}
		 * and will then throw an {@link UnsupportedOperationException}. Use
		 * {@link #supportsRemoteBufferedImage()} to check if this operation is
		 * supported.
		 * 
		 * @return Graphics2D object to draw into this image.
		 * @throws UnsupportedOperationException if this implementation of {@link ImgBase}
		 * does not support this method.
		 * 
		 * @see #supportsRemoteBufferedImage()
		 * @see #paint(Consumer)
		 */
		public default Graphics2D createGraphics(){
			return getRemoteBufferedImage().createGraphics();
		}

		/**
		 * Uses the specified paintInstructions to draw into this image.
		 * This method will pass a {@link Graphics2D} object of this image to the
		 * specified {@link Consumer}. The {@link Consumer#accept(Object)} method
		 * can then draw into this image. When the accept method returns, the
		 * Graphics2D object is disposed.
		 * <p>
		 * This operation may not be supported by an implementation of {@link ImgBase}
		 * and will then throw an {@link UnsupportedOperationException}. Use
		 * {@link #supportsRemoteBufferedImage()} to check if this operation is
		 * supported.
		 * <p>
		 * Example (using lambda expression for Consumers accept method):
		 * <pre>
		 * {@code
		 * Img img = new Img(100, 100);
		 * img.paint( g2d -> { g2d.drawLine(0, 0, 100, 100); } );
		 * }
		 * </pre>
		 * 
		 * @param paintInstructions to be executed on a Graphics2D object of this image
		 * of this Img.
		 * @throws UnsupportedOperationException if this implementation of {@link ImgBase}
		 * does not support this method.
		 * 
		 * @see #createGraphics()
		 * @since 1.3
		 */
		public default void paint(Consumer<Graphics2D> paintInstructions){
			Graphics2D g2d = createGraphics();
			paintInstructions.accept(g2d);
			g2d.dispose();
		}

		/**
		 * Returns an iterator over the pixels of this image. The iterator will
		 * always return the same object on next() but with different index 
		 * (thus referencing different values in the image).
		 * 
		 * @return an iterator over the pixels of this image.
		 * 
		 * @see #iterator(int, int, int, int)
		 * @see #spliterator()
		 * @see #spliterator(int, int, int, int)
		 */
		@Override
		public default Iterator<P> iterator() {
			return new Iterators.ImgIterator<P>(getPixel());
		}

		/**
		 * Returns the minimum number of elements in a split of a {@link Spliterator}
		 * of this Img. Spliterators will only split if they contain more elements than
		 * specified by this value. Default is 1024.
		 * 
		 * @return minimum number of elements of a Spliterator to allow for splitting.
		 */
		public default int getSpliteratorMinimumSplitSize(){return 1024;}

		/**
		 * Returns an {@link Iterator} for the specified area of the image. The Iterator will
		 * always return the same pixel object on next() but with different index 
		 * (thus referencing different values in the image).
		 * 
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @return iterator for iterating over the pixels in the specified area.
		 * @throws IllegalArgumentException if provided area is not within this
		 * image's bounds, or if the area is not positive (width or height &le; 0). 
		 * 
		 * @see #iterator()
		 * @see #spliterator()
		 * @see #spliterator(int, int, int, int)
		 */
		public default Iterator<P> iterator(final int xStart, final int yStart, final int width, final int height) {
			requireAreaInImageBounds(xStart, yStart, width, height, this);
			return new Iterators.ImgAreaIterator<P>(xStart, yStart, width, height, getPixel());
		}

		/**
		 * Returns a {@link Spliterator} over the pixels of this image. Within each
		 * split of the Spliterator a unique pixel object will be used on tryAdvance()
		 * but with changing index (thus referencing different values in the image).
		 * 
		 * @return a Spliterator over the pixels of this image
		 * 
		 * @see #spliterator(int, int, int, int)
		 * @see #rowSpliterator()
		 * @see #colSpliterator()
		 * @see #iterator()
		 * @see #iterator(int, int, int, int)
		 */
		@Override
		public default Spliterator<P> spliterator() {
			return new Iterators.ImgSpliterator<P>(0, numValues()-1, getSpliteratorMinimumSplitSize(),this::getPixel);
		}

		/**
		 * Creates a {@link Spliterator} that guarantees that each split will
		 * at least cover an entire row of the Img. It also guarantes that each
		 * row will be iterated starting at the least index of that row
		 * (e.g.starts at index 0 then continues with index 1, then 2, until
		 * the end of the row, then continuing with the next row).
		 * This Spliterator iterates in row-major order.
		 * 
		 * @return Spliterator that splits at beginning of rows.
		 * 
		 * @see #colSpliterator()
		 * @see #spliterator()
		 * @see #stream(Spliterator, boolean)
		 * @see #stream(PixelConverter, boolean)
		 */
		public default Spliterator<P> rowSpliterator() {
			return new Iterators.RowSpliterator<P>(0, getWidth(), 0, getHeight()-1, this::getPixel);
		}

		/**
		 * Creates a {@link Spliterator} that guarantees that each split will
		 * at least cover an entire column of the Img. It also guarantes that each
		 * column will be iterated starting at the least index of that column
		 * (e.g.starts at index 0 then continues with index 1, then 2, until
		 * the end of the column, then continuing with the next column).
		 * This Spliterator iterates in column-major order.
		 * 
		 * @return Spliterator that splits at beginning of columns.
		 * 
		 * @see #rowSpliterator()
		 * @see #spliterator()
		 * @see #stream(Spliterator, boolean)
		 * @see #stream(PixelConverter, boolean)
		 */
		public default Spliterator<P> colSpliterator() {
			return new Iterators.ColSpliterator<P>(0, getWidth()-1, 0, getHeight(), this::getPixel);
		}

		/**
		 * Creates a {@link Spliterator} over the pixels within the specified area.
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @return spliterator for the specified area.
		 * @throws IllegalArgumentException if provided area is not within this
		 * image's bounds, or if the area is not positive (width or height &le; 0). 
		 * 
		 * @see #spliterator()
		 * @see #colSpliterator()
		 * @see #rowSpliterator()
		 * @see #stream(Spliterator, boolean)
		 * @see #stream(PixelConverter, boolean)
		 */
		public default Spliterator<P> spliterator(final int xStart, final int yStart, final int width, final int height) {
			requireAreaInImageBounds(xStart, yStart, width, height, this);
			return new Iterators.ImgAreaSpliterator<P>(xStart,yStart,width,height, getSpliteratorMinimumSplitSize(), this::getPixel);
		}


		/** 
		 * Default implementation of {@link Iterable#forEach(Consumer)} <br>
		 * only for performance test purposes as it is slower than the
		 * {@link Img#forEach(Consumer)} implementation.
		 * @param action to be performed
		 */
		default void forEach_defaultimpl(final Consumer<? super P> action) {
			Iterable.super.forEach(action);
		}

		/**
		 * Performs the specified action on each of the pixels of this image.
		 * @param action to be performed
		 * 
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 * @since 1.0
		 */
		@Override
		public default void forEach(final Consumer<? super P> action) {
			forEach(false, action);
		}

		/**
		 * Performs the specified action on each of the pixels of this image.
		 * @param parallel whether to be performed in parallel
		 * @param action to be performed
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default void forEach(boolean parallel, final Consumer<? super P> action) {
			if(parallel){
				ParallelForEachExecutor<P> exec = cp.new ParallelForEachExecutor<>(spliterator(), action);
				exec.invoke();
			} else {
				P p = getPixel();
				for(int i = 0; i < numValues(); p.setIndex(++i)){
					action.accept(p);
				}
			}
		}

		/**
		 * Applies the specified action to every pixel in the specified area of this image.
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @param action to be performed on each pixel
		 * @throws IllegalArgumentException if provided area is not within this
		 * images's bounds.
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default void forEach(final int xStart, final int yStart, final int width, final int height, final Consumer<? super P> action) {
			forEach(false, xStart, yStart, width, height, action);
		}

		/**
		 * Applies the specified action to every pixel in the specified area of this image.
		 * @param parallel whether to be performed in parallel
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @param action to be performed on each pixel
		 * @throws IllegalArgumentException if provided area is not within this
		 * images's bounds, or if the area is not positive (width or height &le; 0). 
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default void forEach(boolean parallel, final int xStart, final int yStart, final int width, final int height, final Consumer<? super P> action) {
			requireAreaInImageBounds(xStart, yStart, width, height, this);
			if(parallel){
				ParallelForEachExecutor<P> exec = cp.new ParallelForEachExecutor<>(spliterator(xStart, yStart, width, height), action);
				exec.invoke();
			} else {
				P p = getPixel();
				int yEnd = yStart+height;
				int xEnd = xStart+width;
				for(int y = yStart; y < yEnd; y++){
					for(int x = xStart; x < xEnd; x++){
						p.setPosition(x, y);
						action.accept(p);
					}
				}
			}
		}

		/**
		 * Applies the specified action to every pixel of this image.
		 * Prior to applying the action, each time the pixel is converted using the specified
		 * converter. The action is then performed on an instance of the element type of the converter
		 * (which is also the type accepted by the action).
		 * Finally the modified instance is then converted back to the pixel.
		 * 
		 * @param converter that converts the pixel to the type accepted by the action
		 * @param parallel whether to be performed in parallel
		 * @param action to be performed on each pixel
		 * 
		 * @param <T> converter's element type
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final PixelConverter<? super P,T> converter, boolean parallel, final Consumer<? super T> action) {
			if(parallel){
				Spliterator<T> spliterator = new PixelConvertingSpliterator<>(
						spliterator(),
						converter);
		 		ParallelForEachExecutor<T> exec = cp.new ParallelForEachExecutor<>(spliterator, action);
				exec.invoke();
			} else {
				P px = getPixel();
				T element = converter.allocateElement();
				for(int i = 0; i < numValues(); px.setIndex(++i)){
					converter.convertPixelToElement(px, element);
					action.accept(element);
					converter.convertElementToPixel(element, px);
				}
			}
		}


		/**
		 * Applies the specified action to every pixel in the specified area of this image.
		 * Prior to applying the action, each time the pixel is converted using the specified
		 * converter. The action is then performed on an instance of the element type of the converter
		 * (which is also the type accepted by the action).
		 * Finally the modified instance is then converted back to the pixel.
		 * 
		 * @param converter that converts the pixel to the type accepted by the action
		 * @param parallel whether to be performed in parallel
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @param action to be performed on each pixel
		 * 
		 * @param <T> converter's element type
		 * 
		 * @throws IllegalArgumentException if provided area is not within this
		 * images's bounds, or if the area is not positive (width or height &le; 0). 
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final PixelConverter<? super P, T> converter, boolean parallel, final int xStart, final int yStart, final int width, final int height, final Consumer<? super T> action) {
			requireAreaInImageBounds(xStart, yStart, width, height, this);
			if(parallel){
				Spliterator<T> spliterator = new PixelConvertingSpliterator<>(
						spliterator(xStart, yStart, width, height),
						converter);
				ParallelForEachExecutor<T> exec = cp.new ParallelForEachExecutor<>(spliterator, action);
				exec.invoke();
			} else {
				P p = getPixel();
				T element = converter.allocateElement();
				int yEnd = yStart+height;
				int xEnd = xStart+width;
				for(int y = yStart; y < yEnd; y++){
					for(int x = xStart; x < xEnd; x++){
						p.setPosition(x, y);
						converter.convertPixelToElement(p, element);
						action.accept(element);
						converter.convertElementToPixel(element, p);
					}
				}
			}
		}

		/**
		 * Applies the specified manipulator to every pixel of this image.
		 * @param manipulator that will be applied
		 * @param <T> manipulator's element type
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final PixelManipulator<? super P,T> manipulator) {
			forEach(false, manipulator);
		}

		/**
		 * Applies the specified manipulator to every pixel of this image.
		 * @param parallel whether to be performed in parallel
		 * @param manipulator that will be applied
		 * @param <T> manipulator's element type
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final boolean parallel, final PixelManipulator<? super P,T> manipulator) {
			forEach(manipulator.getConverter(), parallel, manipulator.getAction());
		}

		/**
		 * Applies the specified manipulator to every pixel in the specified area of this image.
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @param manipulator that will be applied
		 * 
		 * @param <T> manipulator's element type
		 * 
		 * @throws IllegalArgumentException if provided area is not within this
		 * images's bounds.
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final int xStart, final int yStart, final int width, final int height, final PixelManipulator<? super P,T> manipulator) {
			forEach(false, xStart, yStart, width, height, manipulator);
		}

		/**
		 * Applies the specified manipulator to every pixel in the specified area of this image.
		 * @param parallel whether to be performed in parallel
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @param manipulator that will be applied
		 * 
		 * @param <T> manipulator's element type
		 * 
		 * @throws IllegalArgumentException if provided area is not within this
		 * images's bounds.
		 * 
		 * @see #forEach(Consumer)
		 * @see #forEach(PixelManipulator)
		 * @see #forEach(boolean, Consumer)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 * @see #forEach(int, int, int, int, Consumer)
		 * @see #forEach(int, int, int, int, PixelManipulator)
		 * @see #forEach(boolean, int, int, int, int, Consumer)
		 * @see #forEach(boolean, int, int, int, int, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, int, int, int, int, Consumer)
		 * @see #stream()
		 */
		public default <T> void forEach(final boolean parallel, final int xStart, final int yStart, final int width, final int height, final PixelManipulator<? super P,T> manipulator) {
			forEach(manipulator.getConverter(), parallel, xStart, yStart, width, height, manipulator.getAction());
		}

		/**
		 * Returns a {@code Stream<Pixel>} for the specified {@code Spliterator<Pixel>}.
		 * This is just a wrapper method arround {@link StreamSupport#stream(Spliterator, boolean)}
		 * mainly used as syntactic sugar for the use with the non default Spliterators
		 * ({@link #rowSpliterator()} and {@link #colSpliterator()}. When the default spliterator
		 * of the Img is sufficient the non-static {@link #stream()} can be used.
		 * <p>
		 * For example (horizontal edge detection in parallel using forward difference):
		 * <pre>
		 * {@code
		 * Img myImg = ...;
		 * ImgBase.stream(myImg.rowSpliterator(), true).forEach( px -> {
		 *     int next = px.getImg().getValue(px.getX()+1, px.getY(), Img.boundary_mode_repeat_edge);
		 *     int forwardDiff = Math.abs( Pixel.getLuminance(next) - px.getLuminance() );
		 *     px.setRGB(forwardDiff, forwardDiff, forwardDiff);
		 * });
		 * }
		 * </pre>
		 * @param spliterator Spliterator of Img to be streamed
		 * @param parallel whether parallel or sequential stream is returned
		 * 
		 * @param <Px> pixel type of the stream
		 * @return a new sequential or parallel pixel stream.
		 *
		 * @see #stream()
		 */
		public static <Px extends PixelBase> Stream<Px> stream(Spliterator<Px> spliterator, boolean parallel){
			return StreamSupport.stream(spliterator, parallel);
		}

		/**
		 * Returns a sequential {@link Stream} of pixels of this image.
		 * This Img's {@link #spliterator()} is used to create the Stream.
		 * <p>
		 * <b>The elements of this stream are not distinct!</b><br>
		 * This is due to a {@link PixelBase} object being a pointer into
		 * the data of the image and not a pixel value itself. While streaming
		 * the index of the pixel object is changed for each actual pixel of the image.
		 * <br>
		 * Thus, a Set created by the expression {@code img.stream().collect(Collectors.toSet())}
		 * will only contain a single element.
		 * 
		 * @return pixel Stream of this image.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(Consumer action)
		 */
		public default Stream<P> stream() {
			return stream(false);
		}

		/**
		 * Returns a {@link Stream} of pixels of this image.
		 * Depending on the specified argument, the stream will be parallel or sequential.
		 * This Img's {@link #spliterator()} is used to create the Stream.
		 * <p>
		 * <b>The elements of this stream are not distinct!</b><br>
		 * This is due to a {@link PixelBase} object being a pointer into
		 * the data of the image and not a pixel value itself. While streaming
		 * the index of the pixel object is changed for each actual pixel of the image.
		 * <br>
		 * Thus, a Set created by the expression {@code img.stream().collect(Collectors.toSet())}
		 * will only contain a single element.
		 * 
		 * @param parallel whether the stream is parallel (true) or sequential (false)
		 * @return pixel Stream of this image.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(Consumer action)
		 */
		public default Stream<P> stream(boolean parallel) {
			return ImgBase.stream(spliterator(), parallel);
		}

		/**
		 * Returns a Pixel {@link Stream} for the specified area of this Img.<br>
		 * This Img's {@link #spliterator(int,int,int,int)} is used to create
		 * the Stream.
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @return Pixel Stream for specified area.
		 * @throws IllegalArgumentException if provided area is not within this
		 * Img's bounds.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(Consumer action)
		 */
		public default Stream<P> stream(final int xStart, final int yStart, final int width, final int height){
			return stream(false, xStart, yStart, width, height);
		}

		/**
		 * Returns a Pixel {@link Stream} for the specified area of this Img.<br>
		 * This Img's {@link #spliterator(int,int,int,int)} is used to create
		 * the Stream.
		 * 
		 * @param parallel whether the stream is parallel (true) or sequential (false)
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * @return Pixel Stream for specified area.
		 * @throws IllegalArgumentException if provided area is not within this
		 * image's bounds.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(Consumer action)
		 */
		public default Stream<P> stream(boolean parallel, final int xStart, final int yStart, final int width, final int height){
			return StreamSupport.stream(spliterator(xStart, yStart, width, height), parallel);
		}

		/**
		 * Returns a {@link Stream} of the specified {@link PixelConverter}'s element type 
		 * over the pixels of this image. Each pixel will be converted to the element type
		 * before being processed, and the element will be back converted to the pixel after
		 * processing. The conversion and back conversion is handled by the specified converter.
		 * 
		 * @param converter that determines the element type of the stream and handles conversion/back conversion
		 * @param parallel whether the stream is parallel (true) or sequential (false)
		 * 
		 * @param <T> converter's element type
		 * @return a Stream over the pixels of this image in the representation given by the specified converter.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(boolean, PixelManipulator)
		 * @see #forEach(PixelConverter, boolean, Consumer)
		 */
		public default <T> Stream<T> stream(PixelConverter<? super P, T> converter, boolean parallel) {
			Spliterator<T> spliterator = new PixelConvertingSpliterator<>(spliterator(), converter);
			return StreamSupport.stream(spliterator, parallel);
		}

		/**
		 * Returns a {@link Stream} of the specified {@link PixelConverter}'s element type 
		 * over the pixels of this image within the specified area. Each pixel will be converted to the element type
		 * before being processed, and the element will be back converted to the pixel after
		 * processing. The conversion and back conversion is handled by the specified converter.
		 * 
		 * @param converter that determines the element type of the stream and handles conversion/back conversion
		 * @param parallel whether the stream is parallel (true) or sequential (false)
		 * @param xStart left boundary of the area (inclusive)
		 * @param yStart upper boundary of the area (inclusive)
		 * @param width of the area
		 * @param height of the area
		 * 
		 * @param <T> converter's element type
		 * 
		 * @return a Stream over the pixels in the specified area of this image 
		 * in the representation given by the specified converter.
		 * @throws IllegalArgumentException if provided area is not within this image's bounds.
		 * 
		 * @see #stream()
		 * @see #stream(boolean parallel)
		 * @see #stream(int x, int y, int w, int h)
		 * @see #stream(boolean parallel, int x, int y, int w, int h)
		 * @see #stream(PixelConverter c, boolean parallel)
		 * @see #stream(PixelConverter c, boolean parallel, int x, int y, int w, int h)
		 * @see #stream(Spliterator spliterator, boolean parallel)
		 * @see #forEach(boolean parallel, int x, int y, int w, int h, PixelManipulator m)
		 * @see #forEach(PixelConverter c, boolean parallel, int x, int y, int w, int h, Consumer action)
		 */
		public default <T> Stream<T> stream(final PixelConverter<? super P, T> converter, boolean parallel, final int xStart, final int yStart, final int width, final int height){
			Spliterator<T> spliterator = new PixelConvertingSpliterator<>(
					spliterator(xStart, yStart, width, height),
					converter);
			return StreamSupport.stream(spliterator, parallel);
		}
		
		/**
		 * Returns a deep copy of this image. 
		 * 'Deep' means that changes made to this image are NOT reflected in the copy.
		 * @return a deep copy.
		 */
		public ImgBase<P> copy();
		
	}

	
	/**
	 * Image class with data stored in an int array.
	 * <p>
	 * In contrast to {@link BufferedImage} the Img class only offers
	 * pixel data to be stored as integer values simplifying data retrieval
	 * and increasing performance due to less overhead and omitting color
	 * model conversions. <br>
	 * However the Img class can be easily used together with BufferedImages
	 * offering convenience methods like {@link #Img(BufferedImage)},
	 * {@link #toBufferedImage()} or {@link #createRemoteImg(BufferedImage)}.
	 * <p>
	 * Moreover the Img class targets lambda expressions introduced in Java 8
	 * useful for per pixel operations by implementing the {@link Iterable}
	 * interface and providing
	 * <ul>
	 * <li> {@link #iterator()} </li>
	 * <li> {@link #spliterator()} </li>
	 * <li> {@link #forEach(Consumer action)} </li>
	 * <li> and {@link #forEach(boolean parallel, Consumer action)}. </li>
	 * </ul>
	 * <p>
	 * Since version 1.1 it is also possible to iterate over a specified area of
	 * the Img using
	 * <ul>
	 * <li> {@link #iterator(int x, int y, int w, int h)} </li>
	 * <li> {@link #spliterator(int x, int y, int w, int h)} </li>
	 * <li> {@link #forEach(int x, int y, int w, int h, Consumer action)} </li>
	 * <li> and {@link #forEach(boolean parallel, int x, int y, int w, int h, Consumer action)}. </li>
	 * </ul>
	 * <p>
	 * Here is an example of a parallelized per pixel operation:
	 * <pre>
	 * {@code
	 * Img img = new Img(1024, 1024);
	 * img.forEach(true, px -> {
	 *     double x = (px.getX()-512)/512.0;
	 *     double y = (px.getY()-512)/512.0;
	 *     double len = Math.max(Math.abs(x),Math.abs(y));
	 *     double angle = (Math.atan2(x,y)+Math.PI)*(180/Math.PI);
	 *
	 *     double r = 255*Math.max(0,1-Math.abs((angle-120)/120.0));
	 *     double g = 255*Math.max(0, 1-Math.abs((angle-240)/120.0));
	 *     double b = 255*Math.max(0, angle <= 120 ?
	 *          1-Math.abs((angle)/120.0):1-Math.abs((angle-360)/120.0));
	 *
	 *     px.setRGB((int)(r*(1-len)), (int)(g*(1-len)), (int)(b*(1-len)));
	 * });
	 * ImageSaver.saveImage(img.getRemoteBufferedImage(), "polar_colors.png");
	 * }</pre>
	 *
	 * @author hageldave
	 * @since 1.0
	 */
	public class Img implements ImgBase<Pixel> {

		/** boundary mode that will return 0 for out of bounds positions.
		 * @see #getValue(int, int, int)
		 * @since 1.0
		 */
		public static final int boundary_mode_zero = 0;

		/** boundary mode that will repeat the edge of of an Img for out of
		 * bounds positions.
		 * @see #getValue(int, int, int)
		 * @since 1.0
		 */
		public static final int boundary_mode_repeat_edge = 1;

		/** boundary mode that will repeat the Img for out of bounds positions.
		 * @see #getValue(int, int, int)
		 * @since 1.0
		 */
		public static final int boundary_mode_repeat_image = 2;

		/** boundary mode that will mirror the Img for out of bounds positions
		 * @see #getValue(int, int, int)
		 * @since 1.0
		 */
		public static final int boundary_mode_mirror = 3;


		/** data array of this Img containing a value for each pixel in row major order
		 * @since 1.0 */
		private final int[] data;

		/** width and height of this image */
		private final int width,height;

		/** minimum number of elements this Img's {@link Spliterator}s can be split to.
		 * Default value is 1024.
		 * @since 1.3
		 */
		private int spliteratorMinimumSplitSize = 1024;


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
		 * Creates a new Img of specified Dimension.
		 * Values are initilaized to 0.
		 * @param dimension extend of the Img (width and height)
		 * @since 1.0
		 */
		public Img(Dimension dimension){
			this(dimension.width,dimension.height);
		}

		/**
		 * Creates a new Img of same dimensions as provided BufferedImage.
		 * Values are copied from argument Image
		 * @param bimg the BufferedImage
		 * @see #createRemoteImg(BufferedImage)
		 * @since 1.0
		 */
		public Img(BufferedImage bimg){
			this(bimg.getWidth(), bimg.getHeight());
			bimg.getRGB(0, 0, this.getWidth(), this.getHeight(), this.getData(), 0, this.getWidth());
		}

		/**
		 * Creates a new Img of specified dimensions.
		 * Provided data array will be used as this images data.
		 * @param width of the Img
		 * @param height of the Img
		 * @param data values (pixels) that will be used as the content of this Img
		 * @throws IllegalArgumentException when the number of pixels of this Img
		 * resulting from width*height does not match the number of provided data values.
		 * @since 1.0
		 */
		public Img(int width, int height, int[] data){
			this(new Dimension(width, height), data);
		}

		/**
		 * Creates a new Img of specified dimensions.
		 * Provided data array will be used as this images data.
		 * @param dim extend of the image (width and height)
		 * @param data values (pixels) that will be used as the content of this Img
		 * @throws IllegalArgumentException when the number of pixels of this Img
		 * resulting from width*height does not match the number of provided data values.
		 * @since 1.0
		 */
		public Img(Dimension dim, int[] data){
			if(dim.width*dim.height != data.length){
				throw new IllegalArgumentException(String.format("Provided Dimension %s does not match number of provided pixels %d", dim, data.length));
			}
			this.width = dim.width;
			this.height = dim.height;
			this.data = data;
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

		/**
		 * Returns the value of this Img at the specified position.
		 * No bounds checks will be performed, positions outside of this
		 * image's dimension can either result in a value for a different position
		 * or an ArrayIndexOutOfBoundsException.
		 * @param x coordinate
		 * @param y coordinate
		 * @return value for specified position
		 * @throws ArrayIndexOutOfBoundsException if resulting index from x and y
		 * is not within the data arrays bounds.
		 * @see #getValue(int, int, int)
		 * @see #getPixel(int, int)
		 * @see #setValue(int, int, int)
		 * @since 1.0
		 */
		public int getValue(final int x, final int y){
			return this.data[y*this.width + x];
		}

		/**
		 * Returns the value of this Img at the specified position.
		 * Bounds checks will be performed and positions outside of this image's
		 * dimensions will be handled according to the specified boundary mode.
		 * <p>
		 * <b><u>Boundary Modes</u></b><br>
		 * {@link #boundary_mode_zero} <br>
		 * will return 0 for out of bounds positions.
		 * <br>
		 * -{@link #boundary_mode_repeat_edge} <br>
		 * will return the same value as the nearest edge value.
		 * <br>
		 * -{@link #boundary_mode_repeat_image} <br>
		 * will return a value of the image as if the if the image was repeated on
		 * all sides.
		 * <br>
		 * -{@link #boundary_mode_mirror} <br>
		 * will return a value of the image as if the image was mirrored on all
		 * sides.
		 * <br>
		 * -<u>other values for boundary mode </u><br>
		 * will be used as default color for out of bounds positions. It is safe
		 * to use opaque colors (0xff000000 - 0xffffffff) and transparent colors
		 * above 0x0000000f which will not collide with one of the boundary modes
		 * (number of boundary modes is limited to 16 for the future).
		 * @param x coordinate
		 * @param y coordinate
		 * @param boundaryMode one of the boundary modes e.g. boundary_mode_mirror
		 * @return value at specified position or a value depending on the
		 * boundary mode for out of bounds positions.
		 * @since 1.0
		 */
		public int getValue(int x, int y, final int boundaryMode){
			if(x < 0 || y < 0 || x >= this.width || y >= this.height){
				switch (boundaryMode) {
				case boundary_mode_zero:
					return 0;
				case boundary_mode_repeat_edge:
					x = (x < 0 ? 0: (x >= this.width ? this.width-1:x));
					y = (y < 0 ? 0: (y >= this.height ? this.height-1:y));
					return getValue(x, y);
				case boundary_mode_repeat_image:
					x = (this.width + (x % this.width)) % this.width;
					y = (this.height + (y % this.height)) % this.height;
					return getValue(x,y);
				case boundary_mode_mirror:
					if(x < 0){ // mirror x to right side of image
						x = -x - 1;
					}
					if(y < 0 ){ // mirror y to bottom side of image
						y = -y - 1;
					}
					x = (x/this.width) % 2 == 0 ? (x%this.width) : (this.width-1)-(x%this.width);
					y = (y/this.height) % 2 == 0 ? (y%this.height) : (this.height-1)-(y%this.height);
					return getValue(x, y);
				default:
					return boundaryMode; // boundary mode can be default color
				}
			} else {
				return getValue(x, y);
			}
		}

		/**
		 * Returns a bilinearly interpolated ARGB value of the image for the
		 * specified normalized position (x and y within [0,1]). Position {0,0}
		 * denotes the image's origin (top left corner), position {1,1} denotes the
		 * opposite corner (pixel at {width-1, height-1}).
		 * <p>
		 * An ArrayIndexOutOfBoundsException may be thrown for x and y greater than 1
		 * or less than 0.
		 * @param xNormalized coordinate within [0,1]
		 * @param yNormalized coordinate within [0,1]
		 * @throws ArrayIndexOutOfBoundsException when a resulting index is out of
		 * the data array's bounds, which can only happen for x and y values less
		 * than 0 or greater than 1.
		 * @return bilinearly interpolated ARGB value.
		 * @since 1.0
		 */
		public int interpolateARGB(final double xNormalized, final double yNormalized){
			double xF = xNormalized * (getWidth()-1);
			double yF = yNormalized * (getHeight()-1);
			int x = (int)xF;
			int y = (int)yF;
			int c00 = getValue(x, 							y);
			int c01 = getValue(x, 						   (y+1 < getHeight() ? y+1:y));
			int c10 = getValue((x+1 < getWidth() ? x+1:x), 	y);
			int c11 = getValue((x+1 < getWidth() ? x+1:x), (y+1 < getHeight() ? y+1:y));
			return interpolateColors(c00, c01, c10, c11, xF-x, yF-y);
		}

		private int interpolateColors(final int c00, final int c01, final int c10, final int c11, final double mx, final double my){
			return argb_fast/*_bounded*/(
					blend( blend(a(c00), a(c10), mx), blend((c01), a(c11), mx), my),
					blend( blend(r(c00), r(c10), mx), blend(r(c01), r(c11), mx), my),
					blend( blend(g(c00), g(c10), mx), blend(g(c01), g(c11), mx), my),
					blend( blend(b(c00), b(c10), mx), blend(b(c01), b(c11), mx), my) );
		}

		private int blend(final int channel1, final int channel2, final double m){
			return (int) ((channel2 * m) + (channel1 * (1.0-m)));
		}

		/**
		 * Creates a new Pixel object for this Img with position {0,0}.
		 * @return a Pixel object for this Img.
		 * @since 1.0
		 */
		public Pixel getPixel(){
			return new Pixel(this, 0);
		}

		/**
		 * Creates a new Pixel object for this Img at specified position.
		 * No bounds checks are performed for x and y.
		 * <p>
		 * <b>Tip:</b><br>
		 * Do not use this method repeatedly while iterating the image.
		 * Use {@link Pixel#setPosition(int, int)} instead to avoid excessive
		 * allocation of Pixel objects.
		 * <p>
		 * You can also use <code>for(Pixel px: img){...}</code> syntax or the
		 * {@link #forEach(Consumer)} method to iterate this image.
		 * @param x coordinate
		 * @param y coordinate
		 * @return a Pixel object for this Img at {x,y}.
		 * @see #getValue(int, int)
		 * @since 1.0
		 */
		public Pixel getPixel(int x, int y){
			return new Pixel(this, x,y);
		}

		/**
		 * Copies specified area of this Img to the specified destination Img
		 * at specified destination coordinates. If destination Img is null a new
		 * Img with the areas size will be created and the destination coordinates
		 * will be ignored so that the Img will contain all the values of the area.
		 * <p>
		 * The specified area has to be within the bounds of this image or
		 * otherwise an IllegalArgumentException will be thrown. Only the
		 * intersecting part of the area and the destination image is copied which
		 * allows for an out of bounds destination area origin.
		 *
		 * @param x area origin in this image (x-coordinate)
		 * @param y area origin in this image (y-coordinate)
		 * @param w width of area
		 * @param h height of area
		 * @param dest destination Img
		 * @param destX area origin in destination Img (x-coordinate)
		 * @param destY area origin in destination Img (y-coordinate)
		 * @return the destination Img
		 * @throws IllegalArgumentException if the specified area is not within
		 * the bounds of this Img or if the size of the area is not positive.
		 * @since 1.0
		 */
		public Img copyArea(int x, int y, int w, int h, Img dest, int destX, int destY){
			requireAreaInImageBounds(x, y, w, h, this);
			if(dest == null){
				return copyArea(x, y, w, h, new Img(w,h), 0, 0);
			}
			if(x==0 && destX==0 && w==dest.getWidth() && w==this.getWidth()){
				if(destY < 0){
					/* negative destination y
					 * need to shrink area by overlap and translate area origin */
					y -= destY;
					h += destY;
					destY = 0;
				}
				// limit area height to not exceed targets bounds
				h = Math.min(h, dest.getHeight()-destY);
				if(h > 0){
					System.arraycopy(this.getData(), y*w, dest.getData(), destY*w, w*h);
				}
			} else {
				if(destX < 0){
					/* negative destination x
					 * need to shrink area by overlap and translate area origin */
					x -= destX;
					w += destX;
					destX = 0;
				}
				if(destY < 0){
					/* negative destination y
					 * need to shrink area by overlap and translate area origin */
					y -= destY;
					h += destY;
					destY = 0;
				}
				// limit area to not exceed targets bounds
				w = Math.min(w, dest.getWidth()-destX);
				h = Math.min(h, dest.getHeight()-destY);
				if(w > 0 && h > 0){
					for(int i = 0; i < h; i++){
						System.arraycopy(
								this.getData(), (y+i)*getWidth()+x,
								dest.getData(), (destY+i)*dest.getWidth()+destX,
								w);
					}
				}
			}
			return dest;
		}

		/**
		 * Sets value at the specified position.
		 * No bounds checks will be performed, positions outside of this
		 * images dimension can either result in a value for a different position
		 * or an ArrayIndexOutOfBoundsException.
		 * @param x coordinate
		 * @param y coordinate
		 * @param value to be set at specified position. e.g. 0xff0000ff for blue color
		 * @throws ArrayIndexOutOfBoundsException if resulting index from x and y
		 * is not within the data arrays bounds.
		 * @see #getValue(int, int)
		 * @since 1.0
		 */
		public void setValue(final int x, final int y, final int value){
			this.data[y*this.width + x] = value;
		}

		/**
		 * Fills the whole image with the specified value.
		 * @param value for filling image
		 * @return this for chaining
		 * @since 1.0
		 */
		public Img fill(final int value){
			Arrays.fill(getData(), value);
			return this;
		}

		/**
		 * @return a deep copy of this Img.
		 * @since 1.0
		 */
		@Override
		public Img copy(){
			return new Img(getDimension(), Arrays.copyOf(getData(), getData().length));
		}

		@Override
		public BufferedImage toBufferedImage(BufferedImage bimg){
			if(bimg.getWidth() != this.getWidth() || bimg.getHeight() != this.getHeight()){
				throw new IllegalArgumentException(String.format(
						"Specified BufferedImage has a different dimension as this image. BufferedImage dimension: [%dx%d], this: [%dx%d]", 
						bimg.getWidth(),bimg.getHeight(), this.getWidth(),this.getHeight()));
			}
			bimg.setRGB(0, 0, getWidth(), getHeight(), getData(), 0, getWidth());
			return bimg;
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
		@Override
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

		@Override
		public boolean supportsRemoteBufferedImage() {
			return true;
		}

		/**
		 * Creates an Img sharing the specified BufferedImage's data. Changes in
		 * the BufferdImage are reflected in the created Img and vice versa.
		 * <p>
		 * Only BufferedImages with DataBuffer of {@link DataBuffer#TYPE_INT} can
		 * be used since the Img class uses an int[] to store its data. An
		 * IllegalArgumentException will be thrown if a BufferedImage with a
		 * different DataBufferType is provided.
		 * @param bimg BufferedImage with TYPE_INT DataBuffer.
		 * @return 
		 * @return Img sharing the BufferedImages data.
		 * @throws IllegalArgumentException if a BufferedImage with a DataBufferType
		 * other than {@link DataBuffer#TYPE_INT} is provided.
		 * @see #getRemoteBufferedImage()
		 * @see #Img(BufferedImage)
		 * @since 1.0
		 */
		public Img createRemoteImg(BufferedImage bimg){
			int type = bimg.getRaster().getDataBuffer().getDataType();
			if(type != DataBuffer.TYPE_INT){
				throw new IllegalArgumentException(
						String.format("cannot create Img as remote of provided BufferedImage!%n"
								+ "Need BufferedImage with DataBuffer of type TYPE_INT (%d). Provided type: %d",
								DataBuffer.TYPE_INT, type));
			}
			Img img = new Img(
					new Dimension(bimg.getWidth(),bimg.getHeight()),
					((DataBufferInt)bimg.getRaster().getDataBuffer()).getData()
				);
			return img;
		}


		/**
		 * Returns the minimum number of elements in a split of a {@link Spliterator}
		 * of this Img. Spliterators will only split if they contain more elements than
		 * specified by this value. Default is 1024.
		 * @return minimum number of elements of a Spliterator to allow for splitting.
		 * @since 1.3
		 */
		@Override
		public int getSpliteratorMinimumSplitSize() {
			return spliteratorMinimumSplitSize;
		}

		/**
		 * Sets the minimum number of elements in a split of a {@link Spliterator}
		 * of this Img. Spliterators will only split if they contain more elements than
		 * specified by this value. Default is 1024.
		 * <p>
		 * It is advised that this number is
		 * chosen carefully and with respect to the Img's size and application of the
		 * spliterator, as it can decrease performance of the parallelized methods<br>
		 * {@link #forEach(boolean parallel, Consumer action)},<br>
		 * {@link #forEach(boolean parallel, int x, int y, int w, int h, Consumer action)} or<br>
		 * {@link #stream(boolean parallel)} etc.<br>
		 * Small values cause a Spliterator to be split more often which will consume more
		 * memory compared to higher values. Special applications on small Imgs using
		 * sophisticated consumers or stream operations may justify the use of small split sizes.
		 * High values cause a Spliterator to be split less often which may cause the work items
		 * to be badly apportioned among the threads and lower throughput.
		 *  
		 * @param size the minimum number of elements a split covers
		 * @throws IllegalArgumentException if specified size is less than 1
		 * @since 1.3
		 */
		public void setSpliteratorMinimumSplitSize(int size) {
			if(size < 1){
				throw new IllegalArgumentException(
						String.format("Minimum split size has to be above zero, specified:%d", size));
			}
			this.spliteratorMinimumSplitSize = size;
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