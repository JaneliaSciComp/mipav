package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;

import gov.nih.mipav.view.renderer.WildMagic.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;
import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import WildMagic.LibGraphics.Shaders.CompiledProgramCatalog;
import WildMagic.LibGraphics.Shaders.ImageCatalog;
import WildMagic.LibGraphics.Shaders.PixelProgramCatalog;
import WildMagic.LibGraphics.Shaders.VertexProgramCatalog;

import com.sun.opengl.util.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;
import javax.swing.event.*;

public class VolumeTriPlanarInterfaceDTI extends VolumeTriPlanarInterface 
implements ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1898957906984534260L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private JPanelDTILoad DTIimageLoadPanel;
    
    private JPanelDTIFiberTrack DTIFiberTrackPanel;
    
    private JPanelDTIParametersPanel DTIparamsPanel;

    /** Eigenvector image **/
    private ModelImage m_kEigenVectorImage;
    /** EigenValue image **/
    private ModelImage m_kEigenValueImage;
    
    /** Anisotropy image **/
    private ModelImage m_kAnisotropyImage;
    
    /** Diffusion Tensor image. */
	private ModelImage m_kDTIImage = null;
	
	/** Diffusion Tensor image. */
	private ModelImage m_kDTIColorImage = null;
	
	private String m_kParentDir;

	private JPanel DTIFiberPanel;
	
	private JPanel DTIParametersPanel;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Make a volume rendering frame, which contains the toolbars on the top, control panel on the left, the volume
     * rendering panel on the right, and the three orthogonal view ( axial, sagittal, coronal, views) on the bottom
     * right.
     *
     * @param  _imageA                First image to display
     * @param  LUTa                   LUT of the imageA (if null grayscale LUT is constructed)
     * @param  _RGBTA                 RGB table of imageA
     * @param  _imageB                Second loaded image
     * @param  LUTb                   LUT of the imageB
     * @param  _RGBTB                 RGB table of imageB
     * @param  _leftPanelRenderMode   shear warp render mode enabled or not
     * @param  _rightPanelRenderMode  volume rendering panel render mode ( Raycast, shearwarp, etc).
     * @param  _resampleDialog        resample dialog reference.
     */
    public VolumeTriPlanarInterfaceDTI(ModelImage _imageA) {
        super();

        try {
            setIconImage(MipavUtil.getIconImage("4plane_16x16.gif"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        imageA = _imageA;
        imageOrientation = imageA.getImageOrientation();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    
    public void setParentDir(String _path) {
       m_kParentDir = _path;
    }
    
    public void setEVimage(ModelImage _m_kEigenVectorImage) {
        m_kEigenVectorImage = _m_kEigenVectorImage;
    }
    
    public void setEValueimage(ModelImage _m_kEigenValueImage) {
        m_kEigenValueImage = _m_kEigenValueImage;
    }
    
    public void setFAimage(ModelImage _m_kAnisotropyImage) {
    	m_kAnisotropyImage = _m_kAnisotropyImage;
    }
    
    public void setDTIimage(ModelImage _m_kDTIImage) {
	    m_kDTIImage = _m_kDTIImage;
    }
	
    
    
    public void setDTIColorImage(ModelImage _m_kDTIColorImage) {
    	m_kDTIColorImage = _m_kDTIColorImage;
    	m_kDTIColorImage.setExtents(_m_kDTIColorImage.getExtents());
    	m_kDTIColorImage.calcMinMax();
    	if ( imageA != null )
    	{
    	    imageA.disposeLocal();
    	    imageA = null;
    	}
    	imageA = _m_kDTIColorImage;
    	

        boolean bDirExists = true;
        m_kParentDir = imageA.getFileInfo()[0].getFileDirectory().concat( File.separator + "RenderFiles" + File.separator);
        File kDir = new File( m_kParentDir );
        if ( !kDir.exists() )
        {
            bDirExists = false;
            try {
                kDir.mkdir();
            } catch (SecurityException e) {}
        }
        m_kVolumeImageA = new VolumeImage( imageA, "A", true, m_kParentDir, 0, null );
        imageA = m_kVolumeImageA.GetImage();
        RGBTA = m_kVolumeImageA.GetRGB();
        this.LUTa = m_kVolumeImageA.GetLUT();
    	
    	restoreContext();
    }
    
    public String getParentDir() {
    	return m_kParentDir;
    }
    
    public ModelImage getEVimage() { 
       return m_kEigenVectorImage;
    }
    
    public ModelImage getEValueimage() { 
       return m_kEigenValueImage;
    }
    
    public ModelImage getFAimage() { 
    	return m_kAnisotropyImage;
    }
    
	public ModelImage getDTIimage() { 
		return m_kDTIImage;
	}
	

    public void buildDTIimageLoadPanel() {
    	JPanel DTIimagePanel = new JPanel();
    	
    	DTIimageLoadPanel = new JPanelDTILoad(this);
    	
    	DTIimagePanel.add(DTIimageLoadPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIimagePanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("DWI / DTI", null, DTIimagePanel);
    }
    
    public void buildDTIFiberTrackPanel() {
    	DTIFiberPanel = new JPanel();

    	DTIFiberTrackPanel = new JPanelDTIFiberTrack(this);
    	
    	DTIFiberPanel.add(DTIFiberTrackPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIFiberPanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("Fiber Tracks", null, DTIFiberPanel);
    }
    
    public void setFiberTrackActive() {
    	insertTab("Fiber Tracks", DTIFiberPanel);
    }
    
    public void setDTIParamsActive() {
    	insertTab("Fibers", DTIParametersPanel);
    	tabbedPane.setSelectedIndex(tabbedPane.indexOfTab("Fibers") );
    }
    
    public JPanelDTIParametersPanel getParamPanel() {
    	return DTIparamsPanel;
    }
    
    public void buildDTIParametersPanel() {
    	DTIParametersPanel = new JPanel();
    	
    	DTIparamsPanel = new JPanelDTIParametersPanel(this, raycastRenderWM);
    	
    	DTIParametersPanel.add(DTIparamsPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIParametersPanel.getPreferredSize().width, maxPanelWidth);
    	
    	tabbedPane.addTab("Fibers", null, DTIParametersPanel);
    }

    /**
     * The histogram control panel of the lookup table.
     */
    public void buildHistoLUTPanel() {
        histoLUTPanel = new JPanel();

        if (imageA.isColorImage()) {
            panelHistoRGB = new JPanelHistoRGB(imageA, imageB, RGBTA, RGBTB, true);
        } else {
            panelHistoLUT = new JPanelHistoLUT(imageA, imageB, LUTa, LUTb, true);
        }

        
        if (imageA.isColorImage()) {
            histoLUTPanel.add(panelHistoRGB.getMainPanel());
        } else {
            histoLUTPanel.add(panelHistoLUT.getMainPanel());
        }

        maxPanelWidth = Math.max(histoLUTPanel.getPreferredSize().width, maxPanelWidth);
    }

    public JPanelLights_WM getLightControl() {
    	return m_kLightsPanel;
    }

    /**
     * Construct the volume rendering methods based on the choices made from the resample dialog. This method is called
     * by the Resample dialog.
     */
    public void constructRenderers() {

        /** Progress bar show up during the volume view frame loading */
        progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                                           null, null);
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(false);
        progressBar.updateValueImmed(1);

        try {

            serif12 = MipavUtil.font12;
            serif12B = MipavUtil.font12B;
            progressBar.updateValueImmed(5);

            String kExternalDirs = getExternalDirs();        
            ImageCatalog.SetActive( new ImageCatalog("Main", kExternalDirs) );      
            VertexProgramCatalog.SetActive(new VertexProgramCatalog("Main", kExternalDirs));       
            PixelProgramCatalog.SetActive(new PixelProgramCatalog("Main", kExternalDirs));
            CompiledProgramCatalog.SetActive(new CompiledProgramCatalog());
            
            m_kAnimator = new Animator();
            m_akPlaneRender = new PlaneRender_WM[3];
            /*
            m_akPlaneRender[0] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.AXIAL,
                                                    false);
            m_akPlaneRender[1] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.SAGITTAL,
                                                    false);
            m_akPlaneRender[2] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                                                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.CORONAL,
                                                    false);
            */
            m_akPlaneRender[0] = new PlaneRenderDTI();
            m_akPlaneRender[1] = new PlaneRenderDTI();
            m_akPlaneRender[2] = new PlaneRenderDTI();
            
            progressBar.setMessage("Constructing gpu renderer...");
            raycastRenderWM = new VolumeTriPlanerRenderDTI();

            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            progressBar.updateValueImmed(100);
            
            this.configureFrame();
        } finally {
            progressBar.dispose();
        }

                
        /*
        if (imageA.isColorImage()) {
            setRGBTA(RGBTA);

            if ((imageB != null) && imageB.isColorImage()) {
                setRGBTB(RGBTB);
            }

            updateImages(true);
        }
        else
        {
            updateImages(true);
        }
        */
        // Toolkit.getDefaultToolkit().setDynamicLayout( false );
    }

    /**
     * Construct the volume rendering methods based on the choices made from the resample dialog. This method is called
     * by the Resample dialog.
     */
    public void restoreContext() {

        /** Progress bar show up during the volume view frame loading */
        progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                                           null, null);
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(false);
        progressBar.updateValueImmed(1);

        try {
            progressBar.updateValueImmed(5);
          
            ((PlaneRenderDTI)m_akPlaneRender[0]).loadImage(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.AXIAL);
			((PlaneRenderDTI)m_akPlaneRender[1]).loadImage(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.SAGITTAL);
			((PlaneRenderDTI)m_akPlaneRender[2]).loadImage(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.CORONAL);
                      
            progressBar.setMessage("Constructing gpu renderer...");
            
            ((VolumeTriPlanerRenderDTI)raycastRenderWM).loadImage(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB);
          
            progressBar.updateValueImmed(80);
            progressBar.setMessage("Constructing Lookup Table...");

            progressBar.updateValueImmed(100);
                        
            tabbedPane.removeAll();
            buildHistoLUTPanel();
            buildOpacityPanel();            
            buildDisplayPanel();            
            buildDTIimageLoadPanel();
            buildDTIParametersPanel();         
            buildLightPanel();
            buildClipPanel();
            buildSlicePanel();
            buildSurfacePanel();
            buildGeodesic();
            buildSculpt();
            buildSurfaceTexturePanel();
            buildMultiHistogramPanel();
            
            panelAxial = new JPanel(new BorderLayout());
            panelAxial.add(m_akPlaneRender[0].GetCanvas(), BorderLayout.CENTER);

            panelSagittal = new JPanel(new BorderLayout());
            panelSagittal.add(m_akPlaneRender[1].GetCanvas(), BorderLayout.CENTER);

            panelCoronal = new JPanel(new BorderLayout());
            panelCoronal.add(m_akPlaneRender[2].GetCanvas(), BorderLayout.CENTER);
            
            triImagePanel.removeAll();
            triImagePanel.add(panelAxial);
            triImagePanel.add(panelSagittal);
            triImagePanel.add(panelCoronal);

            gpuPanel.add(raycastRenderWM.GetCanvas(), BorderLayout.CENTER);
            gpuPanel.setVisible(true);
            raycastRenderWM.setVisible(true);
            
            int triImagePanelWidth = (int) (screenWidth * 0.51f);
            int triImagePanelHeight = (int) (screenHeight * 0.25f);

            triImagePanel.setPreferredSize(new Dimension(triImagePanelWidth, triImagePanelHeight));
            triImagePanel.setMinimumSize(new Dimension(150, 50));
            
            TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
            m_kVolumeImageA.UpdateImages(kTransfer, 0, null);          
            if ( imageB != null )
            {
                kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 0, null);
            }
            progressBar.dispose();
            
            // MUST register frame to image models
            imageA.addImageDisplayListener(this);

            if (imageB != null) {
                imageB.addImageDisplayListener(this);
            }
            
            
            // work around to fix the init panel viewing aspect ratio offset problem. 
            m_kAnimator.start();
            getLightControl().refreshLighting();
            
            // After the whole WM rendering framework built, force updating the color LUT table in order to 
            // update both the volume viewer and tri-planar viewer.  Otherwise, the render volume turns to be black.
            if ( panelHistoLUT != null ) 
                panelHistoLUT.updateComponentLUT();
            
        } finally {
            progressBar.dispose();
        }

       
        if (imageA.isColorImage()) {
            setRGBTA(RGBTA);

            if ((imageB != null) && imageB.isColorImage()) {
                setRGBTB(RGBTB);
            }

            updateImages(true);
        }
        else
        {
            updateImages(true);
        }
    }
    
 
    
    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        super.disposeLocal(flag);

        if ( m_kEigenVectorImage != null )
        {
            m_kEigenVectorImage.disposeLocal();
            m_kEigenVectorImage = null;
        }
        if ( m_kEigenValueImage != null )
        {
            m_kEigenValueImage.disposeLocal();
            m_kEigenValueImage = null;
        }
        if ( m_kAnisotropyImage != null )
        {
            m_kAnisotropyImage.disposeLocal();
            m_kAnisotropyImage = null;
        }
        
        if ( DTIparamsPanel != null ) {
        	DTIparamsPanel.disposeLocal();
        	DTIparamsPanel = null;
        }
        
        if ( DTIimageLoadPanel != null ) {
        	DTIimageLoadPanel.disposeLocal();
        	DTIimageLoadPanel = null;
        }
        
        if ( m_kDTIImage != null )
        {
        	m_kDTIImage.removeImageDisplayListener(this);
            m_kDTIImage.disposeLocal();
            m_kDTIImage = null;
        }
        
        
        
        if ( imageA != null ) {
        	imageA.removeImageDisplayListener(this);
        	imageA.disposeLocal();
        	imageA = null;
        }
        
        if ( imageB != null ) {
        	imageB.removeImageDisplayListener(this);
        	imageB.disposeLocal();
        	imageB = null;
        }
        
        if ( m_kDTIColorImage != null )
        {
        	m_kDTIColorImage.removeImageDisplayListener(this);
            m_kDTIColorImage.disposeLocal();
            m_kDTIColorImage = null;
        }
        
      
        
    }

    /**
     * Constructs main frame structures for image canvas.
     */
    protected void configureFrame() {
        super.configureFrame( false );
        buildDTIimageLoadPanel();
        //buildDTIFiberTrackPanel();
        buildDTIParametersPanel();
    }



    /**
     * Method that resizes the frame and adjusts the rows, columns as needed.
     */
    protected void resizePanel() {
        int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
            panelToolbar.getHeight();
        DTIparamsPanel.resizePanel(maxPanelWidth, height);
        super.resizePanel();        
        panelToolbar.getHeight();
    }
    

    protected void create3DVOI( boolean bIntersection )
    {
        super.create3DVOI(bIntersection);
        DTIparamsPanel.add3DVOI( m_kVOIName );
    }
    
    public void removeSurface(String kSurfaceName)
    {       
        super.removeSurface(kSurfaceName);
        DTIparamsPanel.remove3DVOI( kSurfaceName );
    }
    
}
