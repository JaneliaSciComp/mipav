package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.PlaneRender_WM;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelLights_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceExtractorCubes;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.awt.event.WindowEvent;
import java.io.File;
import java.util.BitSet;

import javax.media.opengl.awt.GLCanvas;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

import com.jogamp.opengl.util.Animator;

public class VolumeTriPlanarInterfaceDTI extends VolumeTriPlanarInterface 
implements ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1898957906984534260L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
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

	private JPanel DTIParametersPanel;
	
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

   

    public VolumeTriPlanarInterfaceDTI( ModelImage colorTensorImage, ModelImage imageB, ModelImage tensorImage, ModelImage eigenVectorImage,
    		ModelImage eigenValueImage, ModelImage fAImage ) {
        super(colorTensorImage, imageB);
        m_kEigenVectorImage = eigenVectorImage;
        m_kEigenValueImage = eigenValueImage;
        m_kAnisotropyImage = fAImage;
        m_kDTIImage = tensorImage; 
        m_kDTIColorImage = colorTensorImage;
        
        buildDTIParametersPanel();

        try {
            setIconImage(MipavUtil.getIconImage("4plane_16x16.gif"));
        } catch (Exception e) {
            e.printStackTrace();
        }
        setSize( 1331, 925 );
        setVisible(true);        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    
    public void buildDTIParametersPanel() {
    	DTIParametersPanel = new JPanel();
    	
    	DTIparamsPanel = new JPanelDTIParametersPanel(this, raycastRenderWM);
    	
    	DTIParametersPanel.add(DTIparamsPanel.getMainPanel());
    	
    	maxPanelWidth = Math.max(DTIParametersPanel.getPreferredSize().width, maxPanelWidth);
    	
    	insertTab("Fibers", DTIParametersPanel);
    }
    
    public void addSurface(final SurfaceState kSurface) {
    	super.addSurface(kSurface);
    	DTIparamsPanel.add3DVOI(kSurface.Name, kSurface, raycastRenderWM.getVolumeSurface(kSurface.Name) );
    }
    
    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        super.disposeLocal(flag);
        
        if ( (m_kEigenVectorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kEigenVectorImage) == null) )
        {
            m_kEigenVectorImage.disposeLocal();
            m_kEigenVectorImage = null;
        }
        if ( (m_kEigenValueImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kEigenValueImage) == null) )
        {
            m_kEigenValueImage.disposeLocal();
            m_kEigenValueImage = null;
        }
        if ( (m_kAnisotropyImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kAnisotropyImage) == null)  )
        {
            m_kAnisotropyImage.disposeLocal();
            m_kAnisotropyImage = null;
        }
        
        if ( DTIparamsPanel != null ) {
        	DTIparamsPanel.disposeLocal();
        	DTIparamsPanel = null;
        }
        
        if ( (m_kDTIImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kDTIImage) == null)  )
        {
        	m_kDTIImage.removeImageDisplayListener(this);
            m_kDTIImage.disposeLocal();
            m_kDTIImage = null;
        }            
                
        if ( (m_kDTIColorImage != null) && (ViewUserInterface.getReference().getFrameContainingImage(m_kDTIColorImage) == null) )
        {
        	m_kDTIColorImage.removeImageDisplayListener(this);
            m_kDTIColorImage.disposeLocal();
            m_kDTIColorImage = null;
        }
        
      
        
    }
    
    public ModelImage getDTIimage() { 
		return m_kDTIImage;
	}
    
    public ModelImage getEValueimage() { 
       return m_kEigenValueImage;
    }
	
    
    
    public ModelImage getEVimage() { 
       return m_kEigenVectorImage;
    }
    
    public ModelImage getFAimage() { 
    	return m_kAnisotropyImage;
    }
    
    public JPanelLights_WM getLightControl() {
    	return m_kLightsPanel;
    }
    
    public JPanelDTIParametersPanel getParamPanel() {
    	return DTIparamsPanel;
    }
    
    public String getParentDir() {
    	return m_kParentDir;
    }
        
    public void removeSurface(String kSurfaceName)
    {       
        super.removeSurface(kSurfaceName);
        DTIparamsPanel.remove3DVOI( kSurfaceName );
    }
    
    public void setDTIimage(ModelImage _m_kDTIImage) {
	    m_kDTIImage = (ModelImage)_m_kDTIImage.clone();
    }
       
    
    public void setEValueimage(ModelImage _m_kEigenValueImage) {
        m_kEigenValueImage = _m_kEigenValueImage;
    }
    
    public void setEVimage(ModelImage _m_kEigenVectorImage) {
        m_kEigenVectorImage = _m_kEigenVectorImage;
    }
    
    public void setFAimage(ModelImage _m_kAnisotropyImage) {
    	m_kAnisotropyImage = _m_kAnisotropyImage;
    }
    
    
    public void setParentDir(String _path) {
       m_kParentDir = _path;
    }
    
    /**
     * Construct the volume rendering methods based on the choices made from
     * the resample dialog. This method is called by the Resample dialog.
     */
    protected void constructRenderers(final ViewJProgressBar progressBar) {

    	initShared();
    	
        progressBar.updateValue(0, true);
        MipavUtil.centerOnScreen(progressBar);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(1);

        progressBar.updateValueImmed(5);

        m_kAnimator = new Animator();
        m_akPlaneRender = new PlaneRender_WM[3];
        m_akPlaneRender[0] = new PlaneRender_WM(new GLCanvas(caps, sharedDrawable.getContext()), this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.AXIAL);
        m_akPlaneRender[1] = new PlaneRender_WM(new GLCanvas(caps, sharedDrawable.getContext()), this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.SAGITTAL);
        m_akPlaneRender[2] = new PlaneRender_WM(new GLCanvas(caps, sharedDrawable.getContext()), this, m_kAnimator, m_kVolumeImageA, m_kVolumeImageB, FileInfoBase.CORONAL);

        progressBar.setMessage("Constructing gpu renderer...");

        raycastRenderWM = new VolumeTriPlanerRenderDTI( sharedRenderer, new GLCanvas(caps, sharedDrawable.getContext()), this, m_kAnimator, m_kVolumeImageA,
                m_kVolumeImageB);

        progressBar.updateValueImmed(80);
        progressBar.setMessage("Constructing Lookup Table...");

        buildImageDependentComponents();

        progressBar.updateValueImmed(100);

        progressBar.dispose();
        
        pack();
        setVisible(true);
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
}
