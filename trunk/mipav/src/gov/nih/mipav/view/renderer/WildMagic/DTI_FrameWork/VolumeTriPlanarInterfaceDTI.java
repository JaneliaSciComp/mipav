package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.MipavInitGPU;
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
	
	  /** Tract input file. */
    private File m_kTractFile = null;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;

    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;

    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;

    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;
    
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
	    m_kDTIImage = (ModelImage)_m_kDTIImage.clone();
    }
	
    
    
    public void setDTIColorImage(ModelImage _m_kDTIColorImage) {
    	
    	m_kDTIColorImage = _m_kDTIColorImage;
    	m_kDTIColorImage.setExtents(_m_kDTIColorImage.getExtents());
    	m_kDTIColorImage.calcMinMax();
    	
    	//imageA = _m_kDTIColorImage;
    	
        boolean bDirExists = true;
        m_kParentDir = _m_kDTIColorImage.getFileInfo()[0].getFileDirectory();
        String kRenderFilesDir = m_kParentDir + File.separator + "RenderFiles" + File.separator;
        File kDir = new File( kRenderFilesDir );
        if ( !kDir.exists() )
        {
            bDirExists = false;
            try {
                kDir.mkdir();
            } catch (SecurityException e) {}
        }

        m_kVolumeImageA = new VolumeImage( _m_kDTIColorImage, "A", !bDirExists, kRenderFilesDir, 0, null );
        m_kVolumeImageB = new VolumeImage();
    	
    	constructRenderers();
        buildDTIParametersPanel();
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
    	
        getParamPanel().setTractParams(m_kTractFile, m_kTractsLimit, m_kTractsMin, m_kTractsMax, m_kTractPath, m_kDTIImage);
    	// getParamPanel().processTractFile();
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
    
    
    public void setTractParams(File _m_kTractFile, JTextField _m_kTractsLimit, JTextField _m_kTractsMin, JTextField _m_kTractsMax, JTextField _m_kTractPath) {
    	m_kTractFile = _m_kTractFile;
    	m_kTractsLimit = _m_kTractsLimit;
    	m_kTractsMin = _m_kTractsMin;
    	m_kTractsMax = _m_kTractsMax;
    	m_kTractPath = _m_kTractPath;
    }
    
    public JPanelLights_WM getLightControl() {
    	return m_kLightsPanel;
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
                
        if ( m_kDTIColorImage != null )
        {
        	m_kDTIColorImage.removeImageDisplayListener(this);
            m_kDTIColorImage.disposeLocal();
            m_kDTIColorImage = null;
        }
        
      
        
    }
    
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.ViewJFrameBase#windowClosing(java.awt.event.WindowEvent)
     */
    public void windowClosing(WindowEvent event) {
        close();
        disposeLocal(true);
        dispose();
    }

    /**
     * Constructs main frame structures for image canvas.
     */
    protected void configureFrame() {
        super.configureFrame( );
        buildDTIimageLoadPanel();
    }

    /**
     * Construct the volume rendering methods based on the choices made from
     * the resample dialog. This method is called by the Resample dialog.
     */
    protected void constructRenderers() {

        /** Progress bar show up during the volume view frame loading */
        ViewJProgressBar progressBar = new ViewJProgressBar("Constructing renderers...", "Constructing renderers...", 0, 100, false,
                null, null);
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(1);

        progressBar.updateValueImmed(5);

        m_kAnimator = new Animator();
        m_akPlaneRender = new PlaneRender_WM[3];
        m_akPlaneRender[0] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.AXIAL);
        m_akPlaneRender[1] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.SAGITTAL);
        m_akPlaneRender[2] = new PlaneRender_WM(this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.CORONAL);

        progressBar.setMessage("Constructing gpu renderer...");

        raycastRenderWM = new VolumeTriPlanerRenderDTI( this, m_kAnimator, m_kVolumeImageA,
                m_kVolumeImageB);

        progressBar.updateValueImmed(80);
        progressBar.setMessage("Constructing Lookup Table...");

        buildImageDependentComponents();

        progressBar.updateValueImmed(100);

        progressBar.dispose();
        
        pack();
        setVisible(true);
        raycastRenderWM.GetCanvas().display();
        m_akPlaneRender[0].GetCanvas().display();
        m_akPlaneRender[1].GetCanvas().display();
        m_akPlaneRender[2].GetCanvas().display();
    }

    /**
     * Method that resizes the frame and adjusts the rows, columns as needed.
     */
    protected void resizePanel() {
        int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
            panelToolbar.getHeight();     
        if ( m_bDependentInterfaceInit )
        {
            DTIparamsPanel.resizePanel(maxPanelWidth, height);
        }
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
