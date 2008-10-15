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

import javax.swing.*;
import javax.swing.event.*;

public class VolumeTriPlanarInterfaceDTI extends VolumeTriPlanarInterface 
implements MouseListener, ItemListener, ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1898957906984534260L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private JPanelDTILoad DTIimageLoadPanel;
    
    private JPanelDTIFiberTrack DTIFiberTrackPanel;
    
    private JPanelDTIParametersPanel DTIparamsPanel;

    /** Eigenvector image **/
    private ModelImage m_kEigenVectorImage;
    
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
    public VolumeTriPlanarInterfaceDTI(ModelImage _imageA, ModelLUT LUTa, ModelRGB _RGBTA, ModelImage _imageB, ModelLUT LUTb,
                                ModelRGB _RGBTB) {
        super(_imageA, LUTa, _RGBTA, _imageB, LUTb, _RGBTB);
        
        RGBTA = _RGBTA;
        RGBTB = _RGBTB;
        this.LUTa = LUTa;
        this.LUTb = LUTb;

        try {
            setIconImage(MipavUtil.getIconImage("4plane_16x16.gif"));
        } catch (Exception e) {
            e.printStackTrace();
        }

        imageOrientation = imageA.getImageOrientation();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------


    
    public void setParentDir(String _path) {
       m_kParentDir = _path;
    }
    
    public void setEVimage(ModelImage _m_kEigenVectorImage) {
    	m_kEigenVectorImage = _m_kEigenVectorImage;
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
    	imageA = _m_kDTIColorImage;
    	
    	  if (imageA.isColorImage()) {
                               int[] RGBExtents = new int[2];
                  RGBExtents[0] = 4;
                  RGBExtents[1] = 256;
                  RGBTA = new ModelRGB(RGBExtents);
             
          } else {
             
                  LUTa = initLUT(imageA);
             
          }

          if (imageB != null) {
              imageB = (ModelImage) (imageB.clone());

              if (imageB.isColorImage()) {
                                      int[] RGBExtents = new int[2];
                      RGBExtents[0] = 4;
                      RGBExtents[1] = 256;
                      RGBTB = new ModelRGB(RGBExtents);
                
              } else {
                     LUTb = new ModelLUT(ModelLUT.GRAY, 256, imageB.getExtents());
                               }
          }
    	
    	restoreContext();
    }
    
    public String getParentDir() {
    	return m_kParentDir;
    }
    
    public ModelImage getEVimage() { 
       return m_kEigenVectorImage;
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
    }
    
    public JPanelDTIParametersPanel getParamPanel() {
    	return DTIparamsPanel;
    }
    
    public void buildDTIParametersPanel() {
    	DTIParametersPanel = new JPanel();
    	
    	DTIparamsPanel = new JPanelDTIParametersPanel(this, ((VolumeTriPlanerRenderDTI)raycastRenderWM));
    	
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
        progressBar.setVisible(true);
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
        progressBar.setVisible(true);
        progressBar.updateValueImmed(1);

        try {
            progressBar.updateValueImmed(5);
           
            m_kVolumeImageA = new VolumeImage(  imageA, LUTa, RGBTA, "A" );
            if ( imageB != null )
            {
                m_kVolumeImageB = new VolumeImage( imageB, LUTb, RGBTB, "B" );
            }
          
            ((PlaneRenderDTI)m_akPlaneRender[0]).loadImage(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.AXIAL,
                    false);
			((PlaneRenderDTI)m_akPlaneRender[1]).loadImage(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
			                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.SAGITTAL,
			                    false);
			((PlaneRenderDTI)m_akPlaneRender[2]).loadImage(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa,
			                    m_kVolumeImageB, imageB, LUTb, FileInfoBase.CORONAL,
			                    false);
                      
            progressBar.setMessage("Constructing gpu renderer...");
            
            ((VolumeTriPlanerRenderDTI)raycastRenderWM).loadImage(this, m_kAnimator, m_kVolumeImageA, imageA, LUTa, RGBTA,
                    m_kVolumeImageB, imageB, LUTb, RGBTB);

          
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
            buildProbePanel();
            buildGeodesic();
            buildSculpt();
            
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

            int triImagePanelWidth = (int) (screenWidth * 0.51f);
            int triImagePanelHeight = (int) (screenHeight * 0.25f);

            triImagePanel.setPreferredSize(new Dimension(triImagePanelWidth, triImagePanelHeight));
            triImagePanel.setMinimumSize(new Dimension(150, 50));
            
            TransferFunction kTransfer = m_kVolOpacityPanel.getCompA().getOpacityTransferFunction();
            m_kVolumeImageA.UpdateImages(kTransfer, 0);          
            if ( imageB != null )
            {
                kTransfer = m_kVolOpacityPanel.getCompB().getOpacityTransferFunction();
                m_kVolumeImageB.UpdateImages(kTransfer, 0);
            }
        } finally {
            progressBar.dispose();
        }

    }
    
 
    
    /**
     * Dispose memory.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        super.disposeLocal(flag);
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
        super.resizePanel();        
        int height = getSize().height - getInsets().top - getInsets().bottom - menuBar.getSize().height -
        panelToolbar.getHeight();
        //if (DTIimageLoadPanel != null) {
        //	DTIimageLoadPanel.resizePanel(maxPanelWidth, height);
        //}
    }
}
