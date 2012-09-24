package gov.nih.mipav.view.renderer.WildMagic.Render;


import static java.lang.System.nanoTime;
import static java.lang.System.out;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmFFT;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmVolumeNormals;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.*;
import java.nio.Buffer;

import javax.media.opengl.GLContext;

import org.jocl.CL;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibGraphics.Rendering.*;
import WildMagic.LibRenderers.OpenGLRenderer.TextureID;

/** *
 *  The VolumeImage class provides an interface between the MIPAV ModelImage and the 2D and 3D Textures used to render
 *  the ModelImage on the GPU. The VolumeImage creates the supporting Texture and GraphicImage objects that pass
 *  the ModelImage data to the GPU. It also creates Texture and GraphicsImage objects for the ModelImage Look-up Table (LUT)
 *  and the ModelImage opacity transfer function.  Other textures that are used for advanced volume rendering, such as
 *  a normal map for surface rendering, and the gradient-magnitude and laplace images for the multi-histogram rendering are
 *  calculated and passed on-demand to the GPU when the user selects these options in the Volume-Renderer user-interface.
 *  
 *  The VolumeImage data structure handles all GPU content for rendering one ModelImage. All Textures and GraphicsImages
 *  are initialized and stored in the VolumeImage. When needed by the renderer they are loaded onto the GPU. Any images
 *  that are derived from the ModelImage: Normal map, Gradient Magnitude, Laplace, are either read from file or calculated 
 *  and then written to file.  The supporting files are stored in a directory on disk located next to the original ModelImage.
 *  The directory is named with the ModelImage name followed by "_RenderFiles".
 *  
 *  
 */
public class VolumeImage implements Serializable {
	/**  */
	private static final long serialVersionUID = -7254697711265907746L;

	/** Reference to ModelImage image */
	private ModelImage m_kImage;

	/** GraphicsImage contains GM opacity transfer function data: */
	private GraphicsImage m_kOpacityMap_GM = null;

	/**
	 * Texture contains texture filter modes and GraphicsImage for opacity transfer function:
	 */
	private Texture m_kOpacityMapTarget_GM = null;

	/** Data storage for volume: */
	private GraphicsImage[] m_kVolume;

	/** Texture object for data: */
	private Texture m_kVolumeTarget;

	/** Data storage for normals: */
	private GraphicsImage[] m_kNormal;
	/**	Set to true if the Normal Map has been initialized. */
	private boolean m_bNormalsInit = false;

	/** Texture object for normal map: */
	private Texture m_kNormalMapTarget;

	/** Texture object for GPU computations: */
	private Texture m_kScratchTarget;

	/** Data storage for color map: */
	private GraphicsImage m_kColorMap;

	/** Texture object for color map: */
	private Texture m_kColorMapTarget;

	/** Data storage for volume gradient magnitude: */
	private GraphicsImage[] m_kVolumeGM;
	/** Set to true if the Gradient Magnitude texture map has been initialized.  */
	private boolean m_bGMInit = false;

	/** Texture object for volume gradient magnitude data: */
	private Texture m_kVolumeGMTarget;

	/** Data storage for surfaces: */
	private GraphicsImage m_kSurfaceImage;

	/** Texture object for surfaces: */
	private Texture m_kSurfaceTarget;

	/** ModelLUT */
	private ModelLUT m_kLUT = null;

	/** ModelRGB */
	private ModelRGB m_kRGBT = null;

	/** Image scale factors for display in 3D */
	private float m_fX = 1, m_fY = 1, m_fZ = 1, m_fMax = 1;

	/** Image name post-fix typically either 'A' or 'B' */
	private String m_kPostfix = null;

	/** Directory for calculated images */
	private String m_kDir = null;

	/** Histogram data for multi-histogram interface */
	private GraphicsImage[] m_kHisto = null;
	/** Set to true when the multi-histogram histogram texture has been initialized. */
	private boolean m_bHistoInit = false;

	/** Texture object for data: */
	private Texture m_kHistoTarget;

	/** Texture coordinates for displaying histogram in 2D */
	private Vector2f[] m_akHistoTCoord = null;

	private float m_fDRRNormalize = 255.0f;

	/** Current position in time (4D data) */
	private int m_iTimeSlice = 0;

	/** Total number of time-slices (4D data) */
	private int m_iTimeSteps = 0;

	/** 3D sub-images (4D data) */
	private ModelImage[] m_akImages;

	private Vector2f[] m_akGradientMagMinMax;

	/* Default Constructor */
	public VolumeImage() {}

	/**
	 * Create a Volume image with the input ModelImage. The supporting images for advanced volume rendering, such as
	 * the normal map, gradient magnitude and laplace images are generated on-demand and stored in a directory for 
	 * later use. The directory is created if it does not already exist, with the ModelImage name + "_RenderFiles" as
	 * the directory name.
	 * 
	 * @param bClone, when true clone the input ModelImage, when false reference the ModelImage
	 * @param kImage input ModelImage
	 * @param kPostfix Postfix for images 'A' or 'B'
	 * @param kProgress progress bar
	 * @param iProgress progress bar increment
	 */
	public VolumeImage(boolean bClone, final ModelImage kImage, final String kPostfix, final ViewJProgressBar kProgress, final int iProgress) {
		m_kPostfix = new String(kPostfix);

		// See if the render files directory exists for this image, if not create it.
		String kImageName = ModelImage.makeImageName(kImage.getFileInfo(0).getFileName(), "");
		try {
			m_kDir = kImage.getFileInfo()[0].getFileDirectory().concat(kImageName + "_RenderFiles" + File.separator);
		} catch ( NullPointerException e )
		{
			m_kDir = new String(kImageName + "_RenderFiles" + File.separator);
		}
		final File kDir = new File(m_kDir);
		if ( !kDir.exists()) {
			try {
				kDir.mkdir();
			} catch (final SecurityException e) {}
		}
		// clone the input image, in the future this might be a reference.
		if ( bClone )
		{
			m_kImage = (ModelImage)kImage.clone();
		}
		else
		{
			m_kImage = kImage;
		}
		// Initialize the Texture maps.
		init(kProgress, iProgress);
	}

	/**
	 * Copy the data from the input GraphicsImage and return a new ModelImage of that data.
	 * Any changes to the GraphicsImage that occur only on the GPU can first be written from
	 * the GPU back into the GraphicsImage CPU data storage. This enables calculations that
	 * are performed on the GPU to be written back into a ModelImage data structure.
	 * 
	 * @param kImage Graphics Image to copy
	 * @param bSwap when true convert from RGBA (graphics format) to ARGB (ModelImage format)
	 * @return new ModelImage from Volume Texture on GPU.
	 */
	public static ModelImage CreateImageFromTexture(final GraphicsImage kImage, final boolean bSwap) {
		final int iXBound = kImage.GetBound(0);
		final int iYBound = kImage.GetBound(1);
		final int iZBound = kImage.GetBound(2);
		final int iSize = iXBound * iYBound * iZBound;
		final int[] extents = new int[] {iXBound, iYBound, iZBound};

		ModelImage kResult = null;
		if (kImage.GetFormat() == GraphicsImage.FormatMode.IT_RGBA8888) {
			byte[] aucData = kImage.GetData();
			if (bSwap) {
				byte bVal = 0;
				aucData = new byte[4 * iXBound * iYBound * iZBound];
				for (int i = 0; i < iSize; i += 4) {
					if (kImage.GetData()[i + 1] > bVal) {
						bVal = kImage.GetData()[i + 1];
					}
					aucData[i] = kImage.GetData()[i + 3];
					aucData[i + 1] = kImage.GetData()[i + 1];
					aucData[i + 2] = kImage.GetData()[i + 2];
					aucData[i + 3] = kImage.GetData()[i];
					System.err.println( kImage.GetData()[i + 3] + " " + kImage.GetData()[i + 1] + " " + kImage.GetData()[i + 2] );
				}
				// System.err.println( bVal );
			}
			try {
				kResult = new ModelImage(ModelStorageBase.ARGB, extents, "");
				kResult.importData(0, aucData, true);
			} catch (final IOException e) {
				e.printStackTrace();
			}
		} else {
			final byte[] aiImageData = kImage.GetData();
			try {
				kResult = new ModelImage(ModelStorageBase.UBYTE, extents, "");
				kResult.importData(0, aiImageData, true);
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
		return kResult;
	}

	/**
	 * Initialize the textures for the color lookup table.
	 * 
	 * @param kLUT the new LUT.
	 * @param kRGBT the new RGB table.
	 * @param kPostfix the string postfix to concatenate to the "ColorMap" image name.
	 * @return GraphicsImage, the new GraphicsImage storing the colormap lookup table.
	 */
	public static GraphicsImage InitColorMap(final ModelLUT kLUT, final ModelRGB kRGBT, final String kPostFix) {
		final byte[] aucData = new byte[256 * 4];
		if (kLUT == null) {
			// ModelImage is Color, initialize the ModelRGB
			ModelLUT.exportIndexedLUTMin(kRGBT, aucData);
		} else {
			// Initialize the ModelLUT
			ModelLUT.exportIndexedLUTMin(kLUT, aucData);
		}
		// Return the new GraphicsImage containing the table data:
		return new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, 256, aucData, new String("ColorMap" + kPostFix));
	}

	/**
	 * Sets the Texture object containing the color lookup table based on the ModelRGB.
	 * 
	 * @param kTexture the Texture object containing the colormap GraphicsImage.
	 * @param kImage the GraphicsImage containing the colormap data.
	 * @param kRGBT the new ModelRGB.
	 */
	public static void SetRGBT(final Texture kTexture, final GraphicsImage kImage, final ModelRGB kRGBT) {
		if (kRGBT == null) {
			return;
		}
		ModelLUT.exportIndexedLUTMin(kRGBT, kImage.GetData());
		kTexture.Reload(true);
	}

	/**
	 * When a ModelImage changes on the CPU, this function is used to update the ModelImage
	 * data on the CPU.
	 * 
	 * @param kImage Modified ModelImage to copy into the GPU Texture and GraphicsImage
	 * @param iTimeSlice time value for 4D image, 0 otherwise
	 * @param kNewImage a new ModelImage (always 3D) that the data or data subset for 4D image can be copied into (when non-null).
	 * @param kVolumeImage GraphicsImage that will hold the ModelImage data
	 * @param kVolumeTexture Texture object containing the GraphicsImage
	 * @param kImageName new image name for the new ModelImage.
	 * @param bSwap when true swap the ARGB (ModelImage) color data representation to a RGBA (GPU) color representation.
	 * @return
	 */
	public static GraphicsImage UpdateData(final ModelImage kImage, final int iTimeSlice, final ModelImage kNewImage,
			final GraphicsImage kVolumeImage, final Texture kVolumeTexture, final String kImageName, 
			final boolean bSwap, final boolean bRescale) {
		GraphicsImage kReturn = kVolumeImage;
		final int iXBound = kImage.getExtents()[0];
		final int iYBound = kImage.getExtents()[1];
		final int iZBound = kImage.getExtents()[2];

		byte[] aucData = null;
		int iSize = iXBound * iYBound * iZBound;
		if (kImage.isColorImage()) {
			iSize *= 4;
			aucData = new byte[iSize];
			try {
				kImage.exportDataUseMask(iTimeSlice * iSize, iSize, bRescale, aucData);
				if (bSwap) {
					for (int i = 0; i < iSize; i += 4) {
						final byte tmp = aucData[i];
						aucData[i] = aucData[i + 1];
						aucData[i + 1] = aucData[i + 2];
						aucData[i + 2] = aucData[i + 3];
						aucData[i + 3] = tmp;
					}
				}
			} catch (final IOException e) {
				e.printStackTrace();
			}
			if (kReturn == null) {
				kReturn = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, aucData,
						kImageName);
			} else {
				kReturn.SetData(aucData, iXBound, iYBound, iZBound);
			}
		} else {
			aucData = new byte[iSize];
			try {
				kImage.exportDataUseMask(iTimeSlice * iSize, iSize, bRescale, aucData);
				// Temporary make the texture an RGBA texture until JOGL2 fixes NPOT textures.
				byte[] aucData2 = new byte[iSize*4];
				for (int i = 0; i < iSize; i++) {
					aucData2[i * 4 + 0] = aucData[i];
					aucData2[i * 4 + 1] = aucData[i];
					aucData2[i * 4 + 2] = aucData[i];
					aucData2[i * 4 + 3] = 1;
				}

				if (kReturn == null) {
					kReturn = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, aucData2,
							kImageName);
				} else {
					kReturn.SetData(aucData2, iXBound, iYBound, iZBound);
				}
			} catch (final IOException e) {
				e.printStackTrace();
			}

		}
		if (kNewImage != null) {
			try {
				kNewImage.importData(0, aucData, true);
			} catch (final IOException e) {}
		}
		if (kVolumeTexture != null) {
			kVolumeTexture.Reload(true);
		}
		return kReturn;
	}


	private void addNormals(final ModelImage kImage, final int iTimeSlice) {
		final int iXBound = kImage.getExtents()[0];
		final int iYBound = kImage.getExtents()[1];
		final int iZBound = kImage.getExtents()[2];

		int iSize = iXBound * iYBound * iZBound;
		byte[] aucData = new byte[iSize * 4];
		try {
			kImage.exportDataUseMask(0, iSize * 4, true, aucData);
			byte[] volumeData = m_kVolume[iTimeSlice].GetData();
			for (int i = 0; i < iSize; i++) {
				volumeData[i*4 + 1] = aucData[i*4 + 1];
				volumeData[i*4 + 2] = aucData[i*4 + 2];
				volumeData[i*4 + 3] = aucData[i*4 + 3];
			}
		} catch (final IOException e) {
			e.printStackTrace();
		}
		m_kVolumeTarget.Reload(true);
	}


	private GraphicsImage createGM_Laplace(final ModelImage kImageGM, final ModelImage kImageL,
			final GraphicsImage kVolumeImage, 
			final int iTimeSlice, final boolean bSwap) {

		GraphicsImage kReturn = kVolumeImage;
		final int iXBound = kImageGM.getExtents()[0];
		final int iYBound = kImageGM.getExtents()[1];
		final int iZBound = kImageGM.getExtents()[2];

		int iSize = iXBound * iYBound * iZBound;
		byte[] aucDataL = new byte[iSize];

		if ( kImageL != null )
		{
			try {
				kImageL.exportDataUseMask(0, iSize, false, aucDataL);
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}

		byte[] aucDataGM = null;
		if (kImageGM.isColorImage()) {
			iSize *= 4;
			aucDataGM = new byte[iSize];
			try {
				kImageGM.exportDataUseMask(0, iSize, false, aucDataGM);
				if (bSwap) {
					for (int i = 0, j = 0; i < iSize; i += 4) {
						aucDataGM[i] = aucDataGM[i + 1];
						aucDataGM[i + 1] = aucDataGM[i + 2];
						aucDataGM[i + 2] = aucDataGM[i + 3];
						if ( kImageL != null )
						{
							aucDataGM[i + 3] = aucDataL[j++];
						}
					}
				}
				kImageGM.importData( 0, aucDataGM, false );
			} catch (final IOException e) {
				e.printStackTrace();
			}
			if (kReturn == null) {
				kReturn = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, aucDataGM,
						kImageGM.getImageName());
			} else {
				kReturn.SetData(aucDataGM, iXBound, iYBound, iZBound);
			}
		} else {
			aucDataGM = new byte[iSize];
			try {
				kImageGM.exportDataUseMask(0, iSize, false, aucDataGM);
				byte[] aucData2 = new byte[iSize*4];
				for (int i = 0; i < iSize; i++) {
					aucData2[i * 4 + 0] = aucDataGM[i];
					aucData2[i * 4 + 1] = aucDataGM[i];
					aucData2[i * 4 + 2] = aucDataGM[i];
					if ( kImageL != null )
					{
						aucData2[i * 4 + 3] = aucDataL[i];
					}
				}

				if (kReturn == null) {
					kReturn = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, aucData2,
							kImageGM.getImageName());
				} else {
					kReturn.SetData(aucData2, iXBound, iYBound, iZBound);
				}
			} catch (final IOException e) {
				e.printStackTrace();
			}

		}
		return kReturn;
	}



	/**
	 * Creates a new GraphicsImage for the input ModelSimpleImage. The ModelSimpleImage data is 
	 * referenced by the new GraphicsImage and will be passed to the GPU as a texture.
	 * @param kImage input ModelSimpleImage.
	 * @param kImageName name for the GraphicsImage.
	 * @return a new GraphcisImage.
	 */
	public static GraphicsImage UpdateData(final ModelSimpleImage kImage, final String kImageName) {
		final GraphicsImage.FormatMode eType = GraphicsImage.FormatMode.IT_L32F;

		if (kImage.nDims == 3) {
			return new GraphicsImage(eType, kImage.extents[0], kImage.extents[1], kImage.extents[2], kImage.data,
					kImageName);
		}
		return new GraphicsImage(eType, kImage.extents[0], kImage.extents[1], 1, kImage.data, kImageName);
	}

	/**
	 * When the LUT changes, this function updates the LUT data on the GPU.
	 * 
	 * @param kColorTexture the color-map Texture object.
	 * @param kColorMap the color-map GraphicsImage object (stores data).
	 * @param kLUT the updated or new LUT.
	 */
	public static void UpdateImages(final Texture kColorTexture, final GraphicsImage kColorMap, final ModelLUT kLUT) {
		if (kLUT == null) {
			return;
		}
		ModelLUT.exportIndexedLUTMin(kLUT, kColorMap.GetData());
		kColorTexture.Reload(true);
	}

	/**
	 * When the ModelImage data is rendered as a solid surface, the Normal map is used in the rendering.
	 * The Normal map is calculated on the GPU by one of the GLSL shader programs. This function is called
	 * after the GPU calculation has finished and the GPU data has been copied into a new ModelImage on the CPU 
	 * the new ModelImage then contains the Normal map information, which is written into a file and
	 * copied into the Normal map GraphicsImage used to render the original ModelImage.
	 * 
	 * @param i current 3D sub-image for 4D data. If the data is 3D this value should be 0.
	 * @param kImage a new ModelImage containing the calculated Normals.
	 */
	public void CopyNormalFiles(final int i, final ModelImage kImage) {
		final String kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), "_Normal_"
				+ i);
		JDialogBase.updateFileInfo( m_kImage, kImage );
		ModelImage.saveImage( kImage, kImageName + ".xml", m_kDir );
		if ( m_kImage.isColorImage() )
		{
			m_kNormal[i] = VolumeImage.UpdateData(kImage, 0, null, m_kNormal[i], m_kNormalMapTarget, kImage.getImageName(),
					true, true);
		}
		else
		{
			addNormals(kImage, i);
		}
	}

	/**
	 * Read the current Volume Texture from the GPU and return a new ModelImage of that data.
	 * 
	 * @return new ModelImage from Volume Texture on GPU.
	 */
	public static ModelImage CreateImageFromTexture(final GraphicsImage kImage) {
		final int[] extents = new int[ kImage.GetDimension() ];
		for ( int i = 0; i < extents.length; i++ )
		{
			extents[i] = kImage.GetBound(i);
		}

		//GraphicsImage.Type eType = kImage.GetType();
		int type = ModelStorageBase.ARGB;
		/*
    	switch ( eType )
    	{
    	case IT_BYTE : type = ModelStorageBase.BYTE; break;
    	case IT_UBYTE : type = ModelStorageBase.UBYTE; break;
    	case IT_SHORT : type = ModelStorageBase.SHORT; break;
    	case IT_USHORT : type = ModelStorageBase.USHORT; break;
    	case IT_INT : type = ModelStorageBase.INTEGER; break;
    	case IT_UINT : type = ModelStorageBase.UINTEGER; break;
    	case IT_LONG : type = ModelStorageBase.LONG; break;
    	case IT_FLOAT : type = ModelStorageBase.FLOAT; break;
    	case IT_DOUBLE : type = ModelStorageBase.DOUBLE; break;
    	}
		 */
		final ModelImage kResult = new ModelImage(type, extents, kImage.GetName() );
		int size = kImage.GetQuantity();
		for ( int i = 0; i < size;i++ )
		{
			kResult.set(i*4 + 1, kImage.GetData()[i*3 + 0]);
			kResult.set(i*4 + 2, kImage.GetData()[i*3 + 1]);
			kResult.set(i*4 + 3, kImage.GetData()[i*3 + 2]);
		}
		return kResult;
	}

	/**
	 * Memory cleanup.
	 */
	public void dispose() {
		if (m_kImage == null) {
			return;
		}
		m_kImage.disposeLocal();
		m_kImage = null;

		for (final GraphicsImage element : m_kVolume) {
			element.dispose();
		}
		m_kVolume = null;
		m_kVolumeTarget.dispose();
		m_kVolumeTarget = null;

		if ( m_kNormal != null )
		{
			for (final GraphicsImage element : m_kNormal) {
				element.dispose();
			}
			m_kNormal = null;
		}
		if ( m_kNormalMapTarget != null )
		{
			m_kNormalMapTarget.dispose();
			m_kNormalMapTarget = null;
		}
		
		m_kScratchTarget.dispose();
		m_kScratchTarget = null;

		m_kColorMap.dispose();
		m_kColorMap = null;
		m_kColorMapTarget.dispose();
		m_kColorMapTarget = null;

		for (final GraphicsImage element : m_kVolumeGM) {
			element.dispose();
		}
		m_kVolumeGM = null;
		m_kVolumeGMTarget.dispose();
		m_kVolumeGMTarget = null;


		m_kOpacityMap_GM.dispose();
		m_kOpacityMap_GM = null;
		m_kOpacityMapTarget_GM.dispose();
		m_kOpacityMapTarget_GM = null;

		if (m_kSurfaceImage != null) {
			m_kSurfaceImage.dispose();
			m_kSurfaceImage = null;
			m_kSurfaceTarget.dispose();
			m_kSurfaceTarget = null;
		}

		m_kLUT = null;
		m_kPostfix = null;
		if ( m_kHisto != null )
		{
			for (final GraphicsImage element : m_kHisto) {
				element.dispose();
			}
			m_kHisto = null;
		}
		m_akHistoTCoord = null;
	}


	/**
	 * This function is called when the user selects the Surface or Composite Surface volume rendering option.
	 * If the normals have already been initialized the function returns. Otherwise the function checks if the
	 * normals are available in a file on disk, and if so if they match the parameters (size, units, resolutions) of
	 * the original ModelImage. If the files match they are used and the Normal map is read from file. Otherwise this
	 * function launches the GPU-based Normal calculation.  That calculation when finished calls the CopyNormalFiles
	 * which writes the calculated normals to disk and updates the Normal map on the GPU for rendering.
	 */
	public void GenerateNormalFiles( VolumeTriPlanarInterface parentFrame ) {
		if ( m_bNormalsInit )
		{
			return;
		}
		// Check if the Normal map exists on disk and if it matches the parameters of the ModelImage. 
		long time = nanoTime();
		for (int i = 0; i < m_iTimeSteps; i++) {
			final String kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), "_Normal_"
					+ i);
			ModelImage kNormal = ReadFromDisk(kImageName + ".xml", m_kDir);
			if ( kNormal != null && !checkImage( kNormal, m_akImages[i] ) )
			{
				kNormal.disposeLocal();
				kNormal = null;
			}
			if ( kNormal != null )
			{
				if ( m_kImage.isColorImage() )
				{
					m_kNormal[i] = VolumeImage.UpdateData(kNormal, i, null, m_kNormal[i], m_kNormalMapTarget, kNormal.getImageName(),
							true, true);
				}
				else
				{
					addNormals(kNormal, i);	
				}
				kNormal.disposeLocal();
				m_bNormalsInit = true;
			}
		}
		time = nanoTime() - time;
		out.println("read normal file took: "+(time/1000000)+"ms");		
		// If the Normal map is still not initialized -- no matching file on disk, then launch the GPU-based
		// Normal map generator.
		long testNormalsTime = nanoTime();
		if ( !m_bNormalsInit )
		{
			for (int i = 0; i < m_iTimeSteps; i++) {
				OpenCLAlgorithmVolumeNormals oclNormals = new OpenCLAlgorithmVolumeNormals( m_akImages[i], CL.CL_DEVICE_TYPE_GPU );
				oclNormals.run();
				ModelImage result = oclNormals.getDestImage();				

				final String kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), "_Normal_"
						+ i);
				JDialogBase.updateFileInfo( m_kImage, result );
				ModelImage.saveImage( result, kImageName + ".xml", m_kDir );
				
				if ( m_kImage.isColorImage() )
				{
					m_kNormal[i] = VolumeImage.UpdateData(result, i, null, m_kNormal[i], m_kNormalMapTarget, result.getImageName(),
							true, true);
				}
				else
				{
					addNormals(result, i);
				}
				
				result.disposeLocal();
			}
			m_bNormalsInit = true;
		}
		testNormalsTime = nanoTime() - testNormalsTime;
		out.println("GPGPU computation took: "+(testNormalsTime/1000000)+"ms");		


	}

	/**
	 * Return the Color Map Texture.
	 * @return Volume color map Texture.
	 */
	public Texture GetColorMapTarget() {
		return m_kColorMapTarget;
	}

	/**
	 * Return the normalization factor for DDR rendering mode.
	 * @return normalization factor for DDR rendering mode.
	 */
	public float getDRRNorm() {
		return m_fDRRNormalize;
	}

	/**
	 * Return the Gradient Magnitude Texture.
	 * @return Gradient Magnitude Texture.
	 */
	public Texture GetGradientMapTarget() {
		return m_kVolumeGMTarget;
	}

	/**
	 * Returns true if the multi-histogram histogram texture has been initialized, false otherwise.
	 * @return  true if the multi-histogram histogram texture has been initialized, false otherwise.
	 */
	public boolean isHistoInit()
	{
		return m_bHistoInit;
	}

	/**
	 * Returns the name of the multi-histogram histogram GraphicsImage.
	 * @return the name of the multi-histogram histogram GraphicsImage.
	 */
	public String GetHistoName() {
		return m_kHisto[m_iTimeSlice].GetName();
	}


	private ModelImage[] m_akHistogram;
	public ModelImage GetHistogram() {

		if ( !m_bHistoInit )
		{
			SetGradientMagnitude(null, true, m_kPostfix);   
		}
		if ( m_akHistogram == null )
		{
			m_akHistogram = new ModelImage[m_akImages.length];
		}
		if ( m_akHistogram[m_iTimeSlice] == null )
		{
			m_akHistogram[m_iTimeSlice] = new ModelImage(ModelStorageBase.INTEGER, new int[]{256,256}, "JointHisto" + m_iTimeSlice);
			try {
				m_akHistogram[m_iTimeSlice].importData(m_kHisto[m_iTimeSlice].GetData());
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return m_akHistogram[m_iTimeSlice];
	}

	/**
	 * Return the texture coordinates for the multi-histogram histogram texture.
	 * @return the texture coordinates for the multi-histogram histogram texture.
	 */
	public Vector2f[] GetHistoTCoords() {
		return m_akHistoTCoord;
	}

	public Vector2f GetGradientMagnitudeMinMax()
	{
		return m_akGradientMagMinMax[m_iTimeSlice];
	}

	public float GetGradientMagnitudeMin()
	{
		return m_akGradientMagMinMax[m_iTimeSlice].X;
	}

	public float GetGradientMagnitudeMax()
	{
		return m_akGradientMagMinMax[m_iTimeSlice].Y;
	}

	/**
	 * Return the ModelImage volume data.
	 * @return ModelImage volume data.
	 */
	public ModelImage GetImage() {
		return m_kImage;
	}

	/**
	 * Return the ModelImage LUT.
	 * @return Volume LUT.
	 */
	public ModelLUT GetLUT() {
		return m_kLUT;
	}

	/**
	 * Return the Normal map Texture.
	 * @return Normal map Texture.
	 */
	public Texture GetNormalMapTarget() {
		return m_kNormalMapTarget;
	}

	public Texture GetScratchTarget() {
		return m_kScratchTarget;
	}

	/**
	 * Return the gradient magnitude opacity transfer function Texture.
	 * @return gradient magnitude opacity transfer function Texture.
	 */
	public Texture GetOpacityMapGMTarget() {
		return m_kOpacityMapTarget_GM;
	}

	/**
	 * Return the postfix for this VolumeImage.
	 * @return postfix for this VolumeImage.
	 */
	public String GetPostfix() {
		return m_kPostfix;
	}

	/**
	 * Return the Volume RGBT.
	 * @return Volume RGBT.
	 */
	public ModelRGB GetRGB() {
		return m_kRGBT;
	}

	/**
	 * The ModelImage Volume max-scale factor.
	 * @return Volume max-scale factor.
	 */
	public float GetScaleMax() {
		return m_fMax;
	}

	/**
	 * The ModelImage Volume x-scale factor.
	 * @return Volume x-scale factor.
	 */
	public float GetScaleX() {
		return m_fX;
	}

	/**
	 * The ModelImage Volume y-scale factor.
	 * @return Volume y-scale factor.
	 */
	public float GetScaleY() {
		return m_fY;
	}

	/**
	 * The ModelImage Volume z-scale factor.
	 * @return Volume z-scale factor.
	 */
	public float GetScaleZ() {
		return m_fZ;
	}

	/**
	 * Return the surface mask Texture.
	 * @return surface mask Texture.
	 */
	public Texture GetSurfaceTarget() {
		return m_kSurfaceTarget;
	}

	/**
	 * Returns the current rendered time-slice for 4D images. Otherwise returns 0.
	 * @return the current rendered time-slice for 4D images. Otherwise returns 0.
	 */
	public int GetTimeSlice() {
		return m_iTimeSlice;
	}

	/**
	 * Return the Texture containing the volume data.
	 * @return Texture containing the volume data.
	 */
	public Texture GetVolumeTarget() {
		return m_kVolumeTarget;
	}

	/**
	 * Return the Buffer containing the volume data, which is stored in the Texture GrapicsImage.
	 * @return Buffer containing the volume data.
	 */
	public Buffer GetVolumeTargetBuffer() {
		return m_kVolumeTarget.GetImage().GetDataBuffer();
	}

	/**
	 * Initialize the GraphicsImage for the opacity lookup table.
	 * 
	 * @param kImage the ModelImage the opacity transfer function applies to.
	 * @param kPostfix the string postfix to concatenate to the "OpacityMap" image name.
	 * @return GraphicsImage, the new GraphicsImage storing opacity lookup table.
	 */
	public GraphicsImage InitOpacityMap(final ModelImage kImage, final String kPostFix) {
		final int iLutHeight = 256;
		final float[] afData = new float[iLutHeight];
		final float fRange = (float) (kImage.getMax() - kImage.getMin());
		final float fStep = fRange / iLutHeight;
		float fDataValue = (float) kImage.getMin();
		for (int i = 0; i < iLutHeight; i++) {
			afData[i] = (float) (iLutHeight * (kImage.getMax() - fDataValue) / fRange);
			fDataValue += fStep;
		}

		return new GraphicsImage(GraphicsImage.FormatMode.IT_L8, iLutHeight, afData,
				new String("OpacityMap" + kPostFix));
	}

	/**
	 * Return true if the Volume image is a color image.
	 * 
	 * @return true if the Volume image is a color image.
	 */
	public boolean IsColorImage() {
		return m_kImage.isColorImage();
	}

	/**
	 * Release the Textures containing the volume data. Once Textures are released, they will be re-loaded onto the GPU
	 * during the next frame.
	 */
	public void ReleaseVolume() {
		m_kVolumeTarget.Release();
	}



	/**
	 * Called when the user selects the Gradient Magnitude option or the Multi-Histogram option
	 * in the Volume Renderer. 
	 * @param kGradientMagnitude pre-computed GradientMagnitude image or null
	 * @param bComputeLaplace when true the Laplace image and multi-histogram histogram Textures are computed.
	 * @param kPostfix GraphicsImage postfix string.
	 */
	public void SetGradientMagnitude(ModelImage kGradientMagnitude, boolean bComputeLaplace, String kPostfix )
	{
		if ( !m_bGMInit )
		{
			for (int i = 0; i < m_iTimeSteps; i++) {
				ModelImage kImageLaplace = null;
				ModelImage kImageGM = getGradientMagnitude( m_akImages[i], i );
				if ( !m_akImages[i].isColorImage() )
				{
					kImageLaplace = getLaplace( m_akImages[i], i );			
					m_kVolumeGM[i] = createGM_Laplace(kImageGM, kImageLaplace, m_kVolumeGM[i], i, true);
				}
				else
				{
					m_kVolumeGM[i] = createGM_Laplace(kImageGM, null, m_kVolumeGM[i], i, true);				
				}


				kImageGM.calcMinMax();
				m_akGradientMagMinMax[i] = new Vector2f( (float)kImageGM.getMin(), (float)kImageGM.getMax() );	

				ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGM);
				if (kImageFrame != null) {
					kImageFrame.close();
				}
				kImageGM.disposeLocal();
				if ( kImageLaplace != null )
				{
					kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageLaplace);
					if (kImageFrame != null) {
						kImageFrame.close();
					}
					kImageLaplace.disposeLocal();
				}
			}
			m_kVolumeGMTarget.SetImage(m_kVolumeGM[m_iTimeSlice]);
			m_kVolumeGMTarget.Reload(true);
			m_bGMInit = true;
		}

		if ( bComputeLaplace )
		{
			GenerateHistogram(m_kVolume, m_kVolumeGM, kPostfix);
		}
	}

	/**
	 * Sets the ModelRGB for the iImage.
	 * 
	 * @param kRGBT new ModelRGB
	 */
	public void SetRGBT(final ModelRGB kRGBT) {
		VolumeImage.SetRGBT(m_kColorMapTarget, m_kColorMap, kRGBT);
		m_kRGBT = kRGBT;
	}

	/**
	 * Sets the time slices for 4D data.
	 * @param iSlice new time slice value.
	 */
	public void SetTimeSlice(final int iSlice) {
		if (m_iTimeSlice != iSlice) {
			m_iTimeSlice = iSlice;
			update4D();
		}
	}

	/**
	 * Updates the current time slice.
	 * @param bForward when true the time advances on step forward or wraps to the beginning. 
	 * When false the time moves backward.
	 */
	public void update4D(final boolean bForward) {
		if (m_iTimeSteps == 1) {
			return;
		}
		if (bForward) {
			m_iTimeSlice++;
		} else {
			m_iTimeSlice--;
		}
		if (m_iTimeSlice >= m_iTimeSteps) {
			m_iTimeSlice = 0;
		}
		if (m_iTimeSlice < 0) {
			m_iTimeSlice = m_iTimeSteps - 1;
		}

		update4D();
	}

	/**
	 * Update the image data.
	 * 
	 * @param kImage the modified ModelImage
	 * @param bCopytoCPU when true the data is copied from the GPU GraphicsImage into the ModelImage
	 */
	public void UpdateData(final ModelImage kImage, final boolean bCopytoCPU) {
		System.err.println( "UPDATEDATA CALLED" );
		m_kImage = kImage;
		if (bCopytoCPU) {
			VolumeImage.UpdateData(m_kImage, m_iTimeSlice, m_akImages[m_iTimeSlice], m_kVolume[m_iTimeSlice],
					m_kVolumeTarget, m_kImage.getImageName(), true, false);
		} else {
			VolumeImage.UpdateData(m_kImage, m_iTimeSlice, null, m_kVolume[m_iTimeSlice], m_kVolumeTarget, m_kImage
					.getImageName(), true, false);
		}
	}

	/**
	 * Update the LUT for the ModelImage.
	 * 
	 * @param kLUT new LUT for ModelImage.
	 */
	public void UpdateImages(final ModelLUT kLUT) {
		if (kLUT != null) {
			VolumeImage.UpdateImages(m_kColorMapTarget, m_kColorMap, kLUT);
			m_kLUT = kLUT;
		}
	}

	/**
	 * Update the transfer function for the image iImage.
	 * 
	 * @param kTransfer the new opacity transfer function
	 * @param iImage the image to modify (0 = volume image, 2 = gradient mag)
	 * @param kImage GradientMagitude image.
	 * @return boolean true when updated, false otherwise.
	 */
	public boolean UpdateImages(final TransferFunction kTransfer, final int iImage, final ModelImage kImage) {
		if (iImage == 0) {
			return UpdateImages2(m_kImage, m_kColorMapTarget, m_kColorMap, kTransfer);
		} else if ( (iImage == 2) && (kImage != null) && (m_kOpacityMapTarget_GM != null) && (m_kOpacityMap_GM != null)) {
			return UpdateImages(kImage, m_kOpacityMapTarget_GM, m_kOpacityMap_GM, kTransfer);
		}
		return false;
	}

	/**
	 * In order to map line integrals of image intensity to RGB colors where each color channel is 8 bits, it is
	 * necessary to make sure that the integrals are in [0,255]. Producing a theoretical maximum value of a line
	 * integral is not tractable in an application. This method constructs an approximate maximum by integrating along
	 * each line of voxels in the image with line directions parallel to the coordinate axes. The 'processRay' call
	 * adjusts the line integrals using the estimate, but still clamps the integrals to 255 since the estimate might not
	 * be the true maximum.
	 * 
	 * @return float Integral normalization factor.
	 */
	protected float computeIntegralNormalizationFactor() {
		final int iXBound = m_kImage.getExtents()[0];
		final int iYBound = m_kImage.getExtents()[1];
		final int iZBound = m_kImage.getExtents()[2];

		byte[] aucData = null;
		int iSize = iXBound * iYBound * iZBound;
		if (m_kImage.isColorImage()) {
			iSize *= 4;
		}

		aucData = new byte[iSize];

		try {
			m_kImage.exportDataUseMask(0, iSize, false, aucData);
		} catch (final IOException e) {
			e.printStackTrace();
		}

		// compute image normalization factor
		int iX, iY, iZ, iBase, iSteps;
		float fMaxIntegral = 0.0f;
		float fTStep, fIntegral;

		// fix y and z, integrate over x
		for (iY = 0; iY < iYBound; iY++) {

			for (iZ = 0; iZ < iZBound; iZ++) {
				iBase = iXBound * (iY + (iYBound * iZ));
				iSteps = iXBound - 1;
				fIntegral = 0.5f * ( (aucData[iBase] & 0x0ff) + (aucData[iBase + iSteps] & 0x0ff));
				fTStep = 1.0f / iSteps;

				for (iX = 1; iX < iSteps; iX++) {
					fIntegral += (aucData[iBase + iX] & 0x0ff);
				}

				fIntegral *= fTStep;

				if (fIntegral > fMaxIntegral) {
					fMaxIntegral = fIntegral;
				}
			}
		}
		final int iXYProduct = iXBound * iYBound;
		// fix x and z, integrate over y
		for (iX = 0; iX < iXBound; iX++) {

			for (iZ = 0; iZ < iZBound; iZ++) {
				iBase = iX + (iXYProduct * iZ);
				iSteps = iYBound - 1;
				fIntegral = 0.5f * ( (aucData[iBase] & 0x0ff) + (aucData[iBase + (iXBound * iSteps)] & 0x0ff));
				fTStep = 1.0f / iSteps;

				for (iY = 1; iY < iSteps; iY++) {
					fIntegral += (aucData[iBase + (iXBound * iY)] & 0x0ff);
				}

				fIntegral *= fTStep;

				if (fIntegral > fMaxIntegral) {
					fMaxIntegral = fIntegral;
				}
			}
		}

		// fix x and y, integrate over z
		for (iX = 0; iX < iXBound; iX++) {

			for (iY = 0; iY < iYBound; iY++) {
				iBase = iX + (iXBound * iY);
				iSteps = iZBound - 1;
				fIntegral = 0.5f * ( (aucData[iBase] & 0x0ff) + (aucData[iBase + (iXYProduct * iSteps)] & 0x0ff));
				fTStep = 1.0f / iSteps;

				for (iZ = 1; iZ < iSteps; iZ++) {
					fIntegral += (aucData[iBase + (iXYProduct * iZ)] & 0x0ff);
				}

				fIntegral *= fTStep;

				if (fIntegral > fMaxIntegral) {
					fMaxIntegral = fIntegral;
				}
			}
		}
		aucData = null;
		return (fMaxIntegral > 0.0f) ? (1.0f / fMaxIntegral) : 0.00f;
	}

	/**
	 * Checks that the two input images match extents, units of measure and resolutions. The images
	 * may had different sizes (3D or 4D) the first 3-dimensions must match.
	 * @param kImage1
	 * @param kImage2
	 * @return true if the images match extends, units and resolutions.
	 */
	public static boolean checkImage(ModelImage kImage1, ModelImage kImage2 )
	{
		for ( int i = 0; i < Math.min( kImage1.getExtents().length, kImage2.getExtents().length ); i++ )
		{
			if ( kImage1.getExtents()[i] != kImage2.getExtents()[i] )
			{
				return false;
			}
			if ( kImage1.getUnitsOfMeasure()[i] != kImage2.getUnitsOfMeasure()[i] )
			{
				return false;
			}
			if ( kImage1.getResolutions(0)[i] != kImage2.getResolutions(0)[i] )
			{
				return false;
			}
		}
		return true;
	}

	/**
	 * Generate 2D histogram from the input image and the gradient-magnitude
	 * 
	 * @param kImage input GraphicsImage containing the ModelImage data
	 * @param kImageGM input GraphcisImage containing the Gradient Magnitude data.
	 * @param kPostFix post-fix for the image name.
	 */
	private void GenerateHistogram(final GraphicsImage[] kImage, final GraphicsImage[] kImageGM, final String kPostFix) {
		int iTMinX = 255, iTMaxX = 0;
		int iTMinY = 255, iTMaxY = 0;
		float max = Float.MIN_VALUE;
		float min = Float.MAX_VALUE;
		m_kHisto = new GraphicsImage[m_iTimeSteps];
		for (int t = 0; t < m_iTimeSteps; t++) {
			float[] afCount = new float[256 * 256];
			for (int i = 0; i < 256 * 256; i++) {
				afCount[i] = 0;
			}

			int a1;
			int a2;
			final byte[] abHistoData = kImageGM[t].GetData();
			final byte[] abData = kImage[t].GetData();
			if (m_kImage.isColorImage()) {
				int iHisto = 0;
				for (int i = 0; i < abData.length; i += 4) {
					int iR = (abData[i]);
					int iG = (abData[i + 1]);
					int iB = (abData[i + 2]);
					//a1 = (iR * 0.299 + iG * 0.587 + iB * 0.114);
					a1 = (iR + iG + iB)/3;
					a1 = (a1 & 0x00ff);

					iR = (abHistoData[i]);
					iG = (abHistoData[i + 1]);
					iB = (abHistoData[i + 2]);
					//a2 = (short) (iR * 0.299 + iG * 0.587 + iB * 0.114);
					a2 = (iR + iG + iB)/3;
					a2 = (a2 & 0x00ff);
					afCount[a1 + a2 * 256] += 1;
					iHisto++;
				}
			}
			else {
				int iHisto = 0;
				for (int i = 0; i < abData.length; i += 4) {                	
					a1 = abData[i];
					a1 = (a1 & 0x00ff);
					a2 = (abHistoData[iHisto]);
					a2 = (a2 & 0x00ff);
					afCount[a1 + a2 * 256] += 1;                   
					iHisto += 4;
				}
			} 
			max = Float.MIN_VALUE;
			min = Float.MAX_VALUE;
			for (int i = 0; i < 256 * 256; ++i) {
				afCount[i] = (float) Math.log(afCount[i]+1);
				max = Math.max(afCount[i], max);
				min = Math.min(afCount[i], min);
			}
			//System.err.println( min + " " + max );
			final byte[] abHisto = new byte[256 * 256];
			//int maxB = Integer.MIN_VALUE;
			//int minB = Integer.MAX_VALUE;
			for (int i = 0; i < 256 * 256; ++i) {
				abHisto[i] = new Float((afCount[i] / max) * 255f).byteValue();
				//maxB = ( iVal > maxB ) ? iVal : maxB;
				//minB = ( iVal < minB ) ? iVal : minB;
			}
			afCount = null;

			int iMinX = 255, iMaxX = 0;
			int iIndex = 0;
			for (int i = 0; i < 256; i++) {
				for (int j = 0; j < 256; j++) {
					iIndex = i * 256 + j;
					if (abHisto[iIndex] > 50) {
						if (iMinX > j) {
							iMinX = j;
						}
						if (j > iMaxX) {
							iMaxX = j;
						}
					}
				}
			}

			int iMinY = 255, iMaxY = 0;
			for (int j = 0; j < 256; j++) {
				for (int i = 0; i < 256; i++) {
					iIndex = i * 256 + j;
					if (abHisto[iIndex] > 50) {
						if (iMinY > i) {
							iMinY = i;
						}
						if (i > iMaxY) {
							iMaxY = i;
						}
					}
				}
			}
			if (iTMinX > iMinX) {
				iTMinX = iMinX;
			}
			if (iTMaxX < iMaxX) {
				iTMaxX = iMaxX;
			}

			if (iTMinY > iMinY) {
				iTMinY = iMinY;
			}
			if (iTMaxY < iMaxY) {
				iTMaxY = iMaxY;
			}

			// iMinX = 0; iMaxX = 255;
			// iMinY = 0; iMaxY = 255;

			m_kHisto[t] = new GraphicsImage(GraphicsImage.FormatMode.IT_L8, 256, 256, (byte[]) null, new String(
					"VolumeImageHisto" + kPostFix));
			m_kHisto[t].SetData(abHisto, 256, 256);
			/*
            ModelImage kTestHisto2D = new ModelImage( ModelStorageBase.UBYTE, new int[]{256,256}, "Histo2D" );
            try {
				kTestHisto2D.importData(abHisto);
			} catch (IOException e) {
				e.printStackTrace();
			}
            kTestHisto2D.calcMinMax();
            new ViewJFrameImage( kTestHisto2D ); */
		}

		m_kHistoTarget = new Texture();
		m_kHistoTarget.SetImage(m_kHisto[0]);
		m_kHistoTarget.SetShared(true);
		m_kHistoTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kHistoTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kHistoTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kHistoTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		iTMinX = 0;
		iTMaxX = Math.max( iTMaxX, iTMaxY );
		m_akHistoTCoord = new Vector2f[4];
		m_akHistoTCoord[0] = new Vector2f(iTMinX / 255.0f, iTMinX / 255.0f);
		m_akHistoTCoord[1] = new Vector2f(iTMaxX / 255.0f, iTMinX / 255.0f);
		m_akHistoTCoord[2] = new Vector2f(iTMaxX / 255.0f, iTMaxX / 255.0f);
		m_akHistoTCoord[3] = new Vector2f(iTMinX / 255.0f, iTMaxX / 255.0f);
		//m_akHistoTCoord[0] = new Vector2f(0f, 0f);
		//m_akHistoTCoord[1] = new Vector2f(1f, 0f);
		//m_akHistoTCoord[2] = new Vector2f(1f, 1f);
		//m_akHistoTCoord[3] = new Vector2f(0f, 1f);
		m_bHistoInit = true;
	}

	/**
	 * Calculates and stores the gradient magnitude images (3D or 4D) for the input image. Or reads from disk.
	 * The data is stored in the GraphicsImage data structures and will be passed to the GPU to use in rendering.
	 * 
	 * @param kImage input image
	 * @param kGradientMagnitude input Gradient Magnitude image, or null.
	 * @param bComputeLaplace when true the Laplace image is also calculated or read from dis.
	private void GradientMagnitudeImage(final ModelImage kImage, ModelImage kGradientMagnitude, 
			boolean bComputeLaplace) {


		if ( !m_bGMInit )
		{
			ModelImage[] kImageGM = new ModelImage[m_iTimeSteps];
			String[] kImageName = new String[m_iTimeSteps];
			for (int i = 0; i < m_iTimeSteps; i++) {
				kImageName[i] = ModelImage.makeImageName(kImage.getFileInfo(0).getFileName(), new String("_GM_" + i));

				if ( kGradientMagnitude != null && checkImage(kImage, kGradientMagnitude ))
				{
					m_kVolumeGM[i] = VolumeImage.UpdateData(kGradientMagnitude, i, null, m_kVolumeGM[i], 
							m_kVolumeGMTarget, m_kVolumeGM[i].GetName(), true, false);
					ModelImage.saveImage( kGradientMagnitude, kImageName[i] + ".xml", m_kDir );
				}
				else
				{
					kImageGM[i] = ReadFromDisk(kImageName[i] + ".xml", m_kDir);
					if ( kImageGM[i] != null && !checkImage(kImage, kImageGM[i] ) )
					{
						kImageGM[i].disposeLocal();
						kImageGM[i] = null;
					}
					if (kImageGM[i] == null) {
						JDialogGradientMagnitude kCalcMagnitude = new JDialogGradientMagnitude(null, m_akImages[i]);
						kCalcMagnitude.setVisible(false);
						kCalcMagnitude.setOutputNewImage(true);
						kCalcMagnitude.setDisplayProgressBar(true);
						kCalcMagnitude.setSeparateThread(false);
						kCalcMagnitude.setSeparable(true);
						kCalcMagnitude.setUseOCL(true);
						kCalcMagnitude.actionPerformed(new ActionEvent(this, 0, "OK"));
						kImageGM[i] = kCalcMagnitude.getResultImage();
						kCalcMagnitude = null;
					}
					if (kImageGM[i] == null) {
						System.err.println("Gradient magnitude calculation returned null");
						m_kVolumeGM[i] = VolumeImage.UpdateData(kImage, i, null, m_kVolumeGM[i], m_kVolumeGMTarget, kImageName[i], true, false);
					} else {
						kImageGM[i].calcMinMax();
						m_akGradientMagMinMax[i] = new Vector2f( (float)kImageGM[i].getMin(), (float)kImageGM[i].getMax() );

						if ( !( bComputeLaplace && !m_kImage.isColorImage() ) )
						{
							m_kVolumeGM[i] = VolumeImage.UpdateData(kImageGM[i], 0, null, m_kVolumeGM[i], m_kVolumeGMTarget, kImageName[i], true, false);
						}
					}
				}
			}


			if ( bComputeLaplace && !m_kImage.isColorImage() )
			{
				for (int i = 0; i < m_iTimeSteps; i++) {
					final String kImageNameL = ModelImage.makeImageName(kImage.getFileInfo(0).getFileName(), new String(
							"_Laplacian_" + i));
					ModelImage kImageGMGM = null;
					kImageGMGM = ReadFromDisk(kImageNameL + ".xml", m_kDir);
					if ( kImageGMGM != null && !checkImage(kImage, kImageGMGM ) )
					{
						kImageGMGM.disposeLocal();
						kImageGMGM = null;
					}
					if (kImageGMGM == null) {
						final JDialogLaplacian kCalcLaplacian = new JDialogLaplacian(null, m_akImages[i]);
						kCalcLaplacian.setVisible(false);
						kCalcLaplacian.setOutputNewImage(true);
						kCalcLaplacian.setDisplayProgressBar(true);
						kCalcLaplacian.setSeparateThread(false);
						kCalcLaplacian.setUseOCL(true);
						kCalcLaplacian.setSeparable(true);
						kCalcLaplacian.actionPerformed(new ActionEvent(this, 0, "OK"));
						kImageGMGM = kCalcLaplacian.getResultImage();
					}
					if (kImageGMGM != null) {
						m_kVolumeGM[i] = createGM_Laplace(kImageGM[i], kImageGMGM, m_kVolumeGM[i], i, true);
					} else {
						m_kVolumeGM[i] = VolumeImage.UpdateData(kImageGM[i], 0, null, m_kVolumeGM[i], m_kVolumeGMTarget, kImageName[i], true, false);
					}
					final ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGMGM);
					if (kImageFrame != null) {
						kImageFrame.close();
					} else if (kImageGMGM != null) {
						kImageGMGM.disposeLocal();
						kImageGMGM = null;
					}
				}
			}

			m_bGMInit = true;
			m_kVolumeGMTarget.SetImage(m_kVolumeGM[0]);
			m_kVolumeGMTarget.Reload(true);
			
			for ( int i = 0; i < kImageGM.length; i++ )
			{
				if (kImageGM[i] != null) {

					kImageGM[i].setImageDirectory( m_kDir );
					kImageGM[i].setImageName( kImageName[i] + ".xml" );
					ModelImage.saveImage(kImageGM[i], kImageName[i] + ".xml", m_kDir );
					

					final ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGM[i]);
					if (kImageFrame != null) {
						kImageFrame.close();
					}
						
					kImageGM[i].disposeLocal();
					kImageGM[i] = null;
				}
			}
		}
	}
	 */
	
	private ModelImage getGradientMagnitude( ModelImage kImage, int i )
	{
		String kImageName = ModelImage.makeImageName(kImage.getFileInfo(0).getFileName(), new String("_GM_" + i));
		ModelImage kImageGM = ReadFromDisk(kImageName + ".xml", m_kDir);
		if ( kImageGM != null && !checkImage(kImage, kImageGM ) )
		{
			kImageGM.disposeLocal();
			kImageGM = null;
		}
		if (kImageGM == null) {
			JDialogGradientMagnitude kCalcMagnitude = new JDialogGradientMagnitude(null, m_akImages[i]);
			kCalcMagnitude.setOutputNewImageType( m_akImages[i].isColorImage() ? ModelStorageBase.ARGB_FLOAT : ModelStorageBase.FLOAT );
			kCalcMagnitude.setVisible(false);
			kCalcMagnitude.setOutputNewImage(true);
			kCalcMagnitude.setDisplayProgressBar(true);
			kCalcMagnitude.setSeparateThread(false);
			kCalcMagnitude.setSeparable(true);
			kCalcMagnitude.setUseOCL(true);
			kCalcMagnitude.actionPerformed(new ActionEvent(this, 0, "OK"));
			kImageGM = kCalcMagnitude.getResultImage();
			kCalcMagnitude = null;

			kImageGM.setImageDirectory( m_kDir );
			kImageGM.setImageName( kImageName + ".xml" );
			JDialogBase.updateFileInfo( kImage, kImageGM );
			ModelImage.saveImage(kImageGM, kImageName + ".xml", m_kDir );

			final ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageGM);
			if (kImageFrame != null) {
				kImageFrame.setVisible(false);
			}
		}
		return kImageGM;
	}
	
	
	private ModelImage getLaplace( ModelImage kImage, int i )
	{
		String kImageName = ModelImage.makeImageName(kImage.getFileInfo(0).getFileName(), new String("_Laplacian_" + i));
		ModelImage kImageL = ReadFromDisk(kImageName + ".xml", m_kDir);
		if ( kImageL != null && !checkImage(kImage, kImageL ) )
		{
			kImageL.disposeLocal();
			kImageL = null;
		}
		if (kImageL == null) {
			JDialogLaplacian kCalcLaplacian = new JDialogLaplacian(null, m_akImages[i]);
			kCalcLaplacian.setVisible(false);
			kCalcLaplacian.setOutputNewImage(true);
			kCalcLaplacian.setDisplayProgressBar(true);
			kCalcLaplacian.setSeparateThread(false);
			kCalcLaplacian.setUseOCL(true);
			kCalcLaplacian.setSeparable(true);
			kCalcLaplacian.actionPerformed(new ActionEvent(this, 0, "OK"));
			kImageL = kCalcLaplacian.getResultImage();
			kCalcLaplacian = null;

			kImageL.setImageDirectory( m_kDir );
			kImageL.setImageName( kImageName + ".xml" );
			JDialogBase.updateFileInfo( kImage, kImageL );
			ModelImage.saveImage(kImageL, kImageName + ".xml", m_kDir );
			final ViewJFrameImage kImageFrame = ViewUserInterface.getReference().getFrameContainingImage(kImageL);
			if (kImageFrame != null) {
				kImageFrame.setVisible(false);
			}
		}
		return kImageL;
	}

	/**
	 * Initialize the VolumeImage with the ModelImage data.
	 * @param kProgress progress bar
	 * @param iProgress progress bar increment
	 */
	private void init(final ViewJProgressBar kProgress, final int iProgress) {
		// Create LUTS for the ModelImage:
		initLUT();
		// Initialize Texture Maps:
		if ( !m_kImage.isColorImage() )
		{
			initImages();
		}
		else
		{
			initImagesColor();
		}

		if (kProgress != null) {
			kProgress.updateValueImmed(kProgress.getValue() + iProgress);
		}
	}

	/**
	 *  Intializes the Textures and GraphicsImages used to render the ModelImage this 
	 *  VolumeImage represents.
	 */
	private void initImages() {
		m_fDRRNormalize = computeIntegralNormalizationFactor();
		// Initialize Color Map GraphicsImage:
		m_kColorMap = VolumeImage.InitColorMap(m_kLUT, m_kRGBT, m_kPostfix);
		// Initialize Opacity Map for the GradientMagnitude image:
		m_kOpacityMap_GM = InitOpacityMap(m_kImage, new String(m_kPostfix + "_GM"));

		final int iXBound = m_kImage.getExtents()[0];
		final int iYBound = m_kImage.getExtents()[1];
		final int iZBound = m_kImage.getExtents()[2];

		/*
		 * Map the ModelImage volume data to a texture image, including for the ModelImage gradient magnitude data:
		 */
		final int[] aiExtents = m_kImage.getExtents();
		final int iNDims = aiExtents.length;
		String kImageName;
		GraphicsImage.FormatMode type = GraphicsImage.FormatMode.IT_RGBA8888 ;


		if (iNDims == 3) { // ModelImage is 3D:
			m_iTimeSteps = 1;
		}
		else {             // ModelImage is 4D:
			m_iTimeSteps = aiExtents[3];
		}
		// A 4D ModelImage is separated into the 3D Volumes:
		m_akImages = new ModelImage[m_iTimeSteps];
		// Allocate a 3D GraphicsImage for each 3D Volume
		m_kVolume = new GraphicsImage[m_iTimeSteps];
		m_kVolumeGM = new GraphicsImage[m_iTimeSteps];
		m_akGradientMagMinMax = new Vector2f[m_iTimeSteps];

		final int[] aiSubset = new int[] {aiExtents[0], aiExtents[1], aiExtents[2]};
		
		for (int i = 0; i < m_iTimeSteps; i++) {

			if ( m_iTimeSteps > 1 )
			{
				// Will generate the ModelImage GraphicsImage representation and separate the 4D ModelImage into
				// the 3D Subset image.
				m_akImages[i] = new ModelImage(m_kImage.getType(), aiSubset, JDialogBase.makeImageName(m_kImage
						.getImageName(), "_" + i));
				JDialogBase.updateFileInfo( m_kImage, m_akImages[i] );
				m_kVolume[i] = VolumeImage.UpdateData(m_kImage, i, m_akImages[i], null, m_kVolumeTarget, m_akImages[i]
						.getImageName(), true, false);
				m_akImages[i].copyFileTypeInfo(m_kImage);
				m_akImages[i].calcMinMax();
			}
			else
			{
				// Already 3D, just generate the GraphicsImage:
				m_akImages[0] = m_kImage;
				m_kVolume[0] = VolumeImage.UpdateData(m_kImage, m_iTimeSlice, null, null, m_kVolumeTarget, m_kImage
						.getImageName(), true, false);
			}

			// Allocate GraphcisImage for Gradient Magnitude Texture:
			kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), 
					new String("_GM_" + i));              
			m_kVolumeGM[i] = new GraphicsImage(type, iXBound, iYBound, iZBound,
					(byte[])null, kImageName);

		}
		// Initialize the Gradient Magnitude Texture and set its GraphicsImage:
		m_kVolumeGMTarget = new Texture();
		m_kVolumeGMTarget.SetImage(m_kVolumeGM[0]);
		m_kVolumeGMTarget.SetShared(true);
		m_kVolumeGMTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kVolumeGMTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeGMTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeGMTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);
		
		// Initialize the ModelImage Texture and set its GraphicsImage:
		m_kVolumeTarget = new Texture();
		m_kVolumeTarget.SetImage(m_kVolume[0]);
		m_kVolumeTarget.SetShared(true);
		m_kVolumeTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kVolumeTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Initialize the ColorMap Texture and set its GraphicsImage:
		m_kColorMapTarget = new Texture();
		m_kColorMapTarget.SetImage(m_kColorMap);
		m_kColorMapTarget.SetShared(true);

		// Initialize the Normal Map Texture and set its GraphicsImage:
		m_kScratchTarget = new Texture();
		m_kScratchTarget.SetImage(new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound,
				(byte[])null, "ScratchBuffer"));
		m_kScratchTarget.SetShared(true);
		m_kScratchTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kScratchTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kScratchTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kScratchTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Initialize the Opacity Map for the Gradient Magnitude Texture and set its GraphicsImage:
		m_kOpacityMapTarget_GM = new Texture();
		m_kOpacityMapTarget_GM.SetImage(m_kOpacityMap_GM);
		m_kOpacityMapTarget_GM.SetShared(true);

		// Initialize the Surface Mask Texture and set its GraphicsImage:
		m_kSurfaceImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, new byte[4* iXBound
		                                                                                                              * iYBound * iZBound], "SurfaceImage");
		m_kSurfaceTarget = new Texture();
		m_kSurfaceTarget.SetImage(m_kSurfaceImage);
		m_kSurfaceTarget.SetShared(true);
		m_kSurfaceTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kSurfaceTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kSurfaceTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kSurfaceTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Calculate the scale factors for rendering the volume with a unit cube:
		InitScale();
	}
	

	private void initImagesColor() {
		m_fDRRNormalize = computeIntegralNormalizationFactor();
		// Initialize Color Map GraphicsImage:
		m_kColorMap = VolumeImage.InitColorMap(m_kLUT, m_kRGBT, m_kPostfix);
		// Initialize Opacity Map for the GradientMagnitude image:
		m_kOpacityMap_GM = InitOpacityMap(m_kImage, new String(m_kPostfix + "_GM"));

		final int iXBound = m_kImage.getExtents()[0];
		final int iYBound = m_kImage.getExtents()[1];
		final int iZBound = m_kImage.getExtents()[2];

		/*
		 * Map the ModelImage volume data to a texture image, including for the ModelImage gradient magnitude data:
		 */
		final int[] aiExtents = m_kImage.getExtents();
		final int iNDims = aiExtents.length;
		String kImageName;
		GraphicsImage.FormatMode type = GraphicsImage.FormatMode.IT_RGBA8888 ;


		if (iNDims == 3) { // ModelImage is 3D:
			m_iTimeSteps = 1;
		}
		else {             // ModelImage is 4D:
			m_iTimeSteps = aiExtents[3];
		}
		// A 4D ModelImage is separated into the 3D Volumes:
		m_akImages = new ModelImage[m_iTimeSteps];
		// Allocate a 3D GraphicsImage for each 3D Volume
		m_kVolume = new GraphicsImage[m_iTimeSteps];
		m_kVolumeGM = new GraphicsImage[m_iTimeSteps];
		m_kNormal = new GraphicsImage[m_iTimeSteps];
		m_akGradientMagMinMax = new Vector2f[m_iTimeSteps];

		final int[] aiSubset = new int[] {aiExtents[0], aiExtents[1], aiExtents[2]};
		for (int i = 0; i < m_iTimeSteps; i++) {

			if ( m_iTimeSteps > 1 )
			{
				// Will generate the ModelImage GraphicsImage representation and separate the 4D ModelImage into
				// the 3D Subset image.
				m_akImages[i] = new ModelImage(m_kImage.getType(), aiSubset, JDialogBase.makeImageName(m_kImage
						.getImageName(), "_" + i));
				JDialogBase.updateFileInfo( m_kImage, m_akImages[i] );
				m_kVolume[i] = VolumeImage.UpdateData(m_kImage, i, m_akImages[i], null, m_kVolumeTarget, m_akImages[i]
						.getImageName(), true, false);
				m_akImages[i].copyFileTypeInfo(m_kImage);
				m_akImages[i].calcMinMax();
			}
			else
			{
				// Already 3D, just generate the GraphicsImage:
				m_akImages[0] = m_kImage;
				m_kVolume[0] = VolumeImage.UpdateData(m_kImage, m_iTimeSlice, null, null, m_kVolumeTarget, m_kImage
						.getImageName(), true, false);
			}
			// Allocate GraphcisImage for Normal Map Texture:
			kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), 
					new String("_Normal_" + i));              
			m_kNormal[i] = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound,
					(byte[])null, kImageName);

			// Allocate GraphcisImage for Gradient Magnitude Texture:
			kImageName = ModelImage.makeImageName(m_kImage.getFileInfo(0).getFileName(), 
					new String("_GM_" + i));              
			m_kVolumeGM[i] = new GraphicsImage(type, iXBound, iYBound, iZBound,
					(byte[])null, kImageName);
		}
		// Initialize the Gradient Magnitude Texture and set its GraphicsImage:
		m_kVolumeGMTarget = new Texture();
		m_kVolumeGMTarget.SetImage(m_kVolumeGM[0]);
		m_kVolumeGMTarget.SetShared(true);
		m_kVolumeGMTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kVolumeGMTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeGMTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeGMTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Initialize the ModelImage Texture and set its GraphicsImage:
		m_kVolumeTarget = new Texture();
		m_kVolumeTarget.SetImage(m_kVolume[0]);
		m_kVolumeTarget.SetShared(true);
		m_kVolumeTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kVolumeTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kVolumeTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Initialize the ColorMap Texture and set its GraphicsImage:
		m_kColorMapTarget = new Texture();
		m_kColorMapTarget.SetImage(m_kColorMap);
		m_kColorMapTarget.SetShared(true);

		// Initialize the Normal Map Texture and set its GraphicsImage:
		m_kNormalMapTarget = new Texture();
		m_kNormalMapTarget.SetImage(m_kNormal[0]);
		m_kNormalMapTarget.SetShared(true);
		m_kNormalMapTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kNormalMapTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kNormalMapTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kNormalMapTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);


		// Initialize the Normal Map Texture and set its GraphicsImage:
		m_kScratchTarget = new Texture();
		m_kScratchTarget.SetImage(new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound,
				(byte[])null, "ScratchBuffer"));
		m_kScratchTarget.SetShared(true);
		m_kScratchTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kScratchTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kScratchTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kScratchTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Initialize the Opacity Map for the Gradient Magnitude Texture and set its GraphicsImage:
		m_kOpacityMapTarget_GM = new Texture();
		m_kOpacityMapTarget_GM.SetImage(m_kOpacityMap_GM);
		m_kOpacityMapTarget_GM.SetShared(true);

		// Initialize the Surface Mask Texture and set its GraphicsImage:
		m_kSurfaceImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, iXBound, iYBound, iZBound, new byte[4* iXBound
		                                                                                                              * iYBound * iZBound], "SurfaceImage");
		m_kSurfaceTarget = new Texture();
		m_kSurfaceTarget.SetImage(m_kSurfaceImage);
		m_kSurfaceTarget.SetShared(true);
		m_kSurfaceTarget.SetFilterType(Texture.FilterType.LINEAR);
		m_kSurfaceTarget.SetWrapType(0, Texture.WrapType.CLAMP_BORDER);
		m_kSurfaceTarget.SetWrapType(1, Texture.WrapType.CLAMP_BORDER);
		m_kSurfaceTarget.SetWrapType(2, Texture.WrapType.CLAMP_BORDER);

		// Calculate the scale factors for rendering the volume with a unit cube:
		InitScale();
	}

	/**
	 * Create a new LUT for the input image.
	 * 
	 * @param kImage ModelImage.
	 */
	private void initLUT() {

		if (m_kImage.isColorImage()) {
			final float[] x = new float[4];
			final float[] y = new float[4];
			final Dimension dim = new Dimension(256, 256);

			// Set ModelRGB min max values;
			x[0] = 0;
			y[0] = dim.height - 1;

			x[1] = 255 * 0.333f;
			y[1] = (dim.height - 1) - ( (dim.height - 1) / 3.0f);

			x[2] = 255 * 0.667f;
			y[2] = (dim.height - 1) - ( (dim.height - 1) * 0.67f);

			x[3] = 255;
			y[3] = 0;

			final int[] RGBExtents = new int[2];
			RGBExtents[0] = 4;
			RGBExtents[1] = 256;
			m_kRGBT = new ModelRGB(RGBExtents);
			m_kRGBT.getRedFunction().importArrays(x, y, 4);
			m_kRGBT.getGreenFunction().importArrays(x, y, 4);
			m_kRGBT.getBlueFunction().importArrays(x, y, 4);
			m_kRGBT.makeRGB( -1);
		} else {
			final int[] dimExtentsLUT = new int[2];

			dimExtentsLUT[0] = 4;
			dimExtentsLUT[1] = 256;

			m_kLUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

			float min, max;

			if (m_kImage.getType() == ModelStorageBase.UBYTE) {
				min = 0;
				max = 255;
			} else if (m_kImage.getType() == ModelStorageBase.BYTE) {
				min = -128;
				max = 127;
			} else {
				min = (float) m_kImage.getMin();
				max = (float) m_kImage.getMax();
			}

			final float imgMin = (float) m_kImage.getMin();
			final float imgMax = (float) m_kImage.getMax();

			m_kLUT.resetTransferLine(min, imgMin, max, imgMax);
		}
	}

	/**
	 * Initialize the scale factors. Based on the ModelImage Volume.
	 */
	private void InitScale() {
		final float fMaxX = (m_kImage.getExtents()[0] - 1) * m_kImage.getFileInfo(0).getResolutions()[0];
		final float fMaxY = (m_kImage.getExtents()[1] - 1) * m_kImage.getFileInfo(0).getResolutions()[1];
		final float fMaxZ = (m_kImage.getExtents()[2] - 1) * m_kImage.getFileInfo(0).getResolutions()[2];

		m_fMax = fMaxX;
		if (fMaxY > m_fMax) {
			m_fMax = fMaxY;
		}
		if (fMaxZ > m_fMax) {
			m_fMax = fMaxZ;
		}
		m_fX = fMaxX / m_fMax;
		m_fY = fMaxY / m_fMax;
		m_fZ = fMaxZ / m_fMax;
	}

	/**
	 * Reads an image from disk.
	 * 
	 * @param kImageName image name
	 * @param kDir directory
	 * @return ModelImage
	 */
	private ModelImage ReadFromDisk(final String kImageName, final String kDir) {

		final File kFile = new File(kDir, kImageName);
		if ( !kFile.exists()) {
			return null;
		}

		final FileIO fileIO = new FileIO();
		return fileIO.readImage( kImageName, kDir );
		//return fileIO.readXML(kImageName + ".xml", kDir, false, false);
	}

	private void readObject(final java.io.ObjectInputStream in) throws IOException, ClassNotFoundException {
		m_kDir = (String) in.readObject();
		if ( !m_kDir.equals("null")) {
			final String kImageName = (String) in.readObject();
			m_kPostfix = (String) in.readObject();
			m_kImage = ReadFromDisk(kImageName, m_kDir);
			init(null, 0);
		}
	}


	/**
	 * Go to the next 3D volume sub-image for the 4D animation. 
	 * Updates the Textures and causes them to be reloaded onto the GPU.
	 */
	private void update4D() {
		m_kVolumeTarget.SetImage(m_kVolume[m_iTimeSlice]);
		m_kVolumeTarget.Reload(true);
		if ( m_bGMInit )
		{
			m_kVolumeGMTarget.SetImage(m_kVolumeGM[m_iTimeSlice]);
			m_kVolumeGMTarget.Reload(true);        	
		}
		if ( m_bNormalsInit && m_kImage.isColorImage() )
		{
			m_kNormalMapTarget.SetImage(m_kNormal[m_iTimeSlice]);
			m_kNormalMapTarget.Reload(true);
		}
		if ( m_bHistoInit )
		{
			m_kHistoTarget.SetImage(m_kHisto[m_iTimeSlice]);
			m_kHistoTarget.Reload(true);
		}

		m_kImage.setTimeSlice(m_iTimeSlice);
	}

	/**
	 * Called when the opacity transfer function changes. This function updates the Texture
	 * and causes the data to be reloaded onto the GPU.
	 * 
	 * @param kImage the ModelImage the transfer function applies to.
	 * @param kOpacityTexture the opacity Texture passed to the GPU
	 * @param kOpacityMap the opacity data stored in the GraphicsImage
	 * @param kTransfer the new transfer function.
	 */
	private boolean UpdateImages(final ModelImage kImage, final Texture kOpacityTexture,
			final GraphicsImage kOpacityMap, final TransferFunction kTransfer) {
		final int iLutHeight = 256;
		final float[] afData = kOpacityMap.GetFloatData();

		final float fRange = (float) (kImage.getMax() - kImage.getMin());
		final float fStep = fRange / iLutHeight;
		float fDataValue = (float) kImage.getMin();
		for (int i = 0; i < iLutHeight; i++) {
			afData[i] = (kTransfer.getRemappedValue(fDataValue, iLutHeight) / 255.0f);
			fDataValue += fStep;
		}
		kOpacityTexture.Reload(true);
		return true;
	}

	/**
	 * Update the opacity transfer function.
	 * 
	 * @param kImage the ModelImage the transfer function applies to.
	 * @param kOpacityTexture the opacity Texture passed to the GPU
	 * @param kOpacityMap the opacity data stored in the GraphicsImage
	 * @param kTransfer the new transfer function.
	 */
	private boolean UpdateImages2(final ModelImage kImage, final Texture kOpacityTexture,
			final GraphicsImage kOpacityMap, final TransferFunction kTransfer) {
		final int iLutHeight = kOpacityMap.GetBound(0);
		final byte[] abData = kOpacityMap.GetData();

		final float fRange = (float) (kImage.getMax() - kImage.getMin());
		final float fStep = fRange / iLutHeight;
		float fDataValue = (float) kImage.getMin();
		float fVal;
		for (int i = 0; i < iLutHeight; i++) {
			fVal = (kTransfer.getRemappedValue(fDataValue, iLutHeight) / 255.0f);
			abData[i * 4 + 3] = (byte) (fVal * 255);
			fDataValue += fStep;
		}
		kOpacityTexture.Reload(true);
		return true;
	}

	private void writeObject(final java.io.ObjectOutputStream out) throws IOException {
		if (m_kImage != null) {
			out.writeObject(m_kDir);
			out.writeObject(m_kImage.getImageFileName());
			out.writeObject(m_kPostfix);
			m_kImage.saveImage(m_kDir, m_kImage.getImageFileName(), FileUtility.XML, false, false);
		} else {
			out.writeObject("null");
		}
	}
}
