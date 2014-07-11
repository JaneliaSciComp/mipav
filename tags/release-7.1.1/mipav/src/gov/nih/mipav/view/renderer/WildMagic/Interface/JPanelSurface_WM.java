package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogSmoothMesh;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import gov.nih.mipav.view.renderer.WildMagic.Decimate.TriangleMesh;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Random;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.border.EmptyBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;


import WildMagic.LibFoundation.Mathematics.ColorRGB;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.ConvexHull3f;
import WildMagic.LibFoundation.Meshes.VETMesh;
import WildMagic.LibGraphics.Detail.ClodMesh;
import WildMagic.LibGraphics.Rendering.MaterialState;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.Polyline;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;


public class JPanelSurface_WM extends JInterfaceBase
        implements ListSelectionListener, ChangeListener {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4600563188022683359L;

    /** The colors for the surfaces. */
	private static ColorRGB[] fixedColor;

    /** The area label. */
    private JLabel areaLabel;

    /** Displays the area of triangle. */
    private JTextField areaText;

    /** The color button, which calls a color chooser. */
    private JButton colorButton;

    /** The color button label. */
    private JLabel colorLabel;

    /** The polygon mode combo box label. */
    private JLabel comboLabel;

    /** Decimate button. */
    private JButton decimateButton;

    /** The level of detail slider label. */
    private JLabel detailLabel;

    /** Level of detail slider. */
    private JSlider detailSlider;

    /** The labels below the detail slider. */
    private JLabel[] detailSliderLabels;

    /** Save surface button. */
    private JButton saveSurfaceButton;
    
    /** Save .PLY surface button. */
    private JButton savePLYSurfaceButton;
 
    /** Save .STL surface button. */
    private JButton saveSTLSurfaceButton;
    
    /** Paint tool-bar (contained in the SurfacePaint class) */
    private SurfacePaint_WM m_kSurfacePaint = null;

    /** The material options button, which launches the material editor window. */
    private JButton m_kAdvancedMaterialOptionsButton;

    /** Opens SurfaceTexture dialog:. */
    private JButton m_kSurfaceTextureButton;

    /** The opacity slider label. */
    private JLabel opacityLabel;

    /** Opacity slider, not enabled yet. */
    private JSlider opacitySlider;

    /** The labels below the opacity slider. */
    private JLabel[] opacitySliderLabels;

    /** The combo box for the polygon mode to display. */
    private JComboBox polygonModeCB;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Smooth button. */
    private JButton invertNormals, smooth1Button, smooth2Button, smooth3Button, convexHull, subdivideTriangles, extractConnected;

    /** Polyline list box in the dialog for surfaces. */
    private JList polylineList;
    
    /** The list box in the dialog for surfaces. */
    private JList surfaceList;

    /** Check Box for surface picking. */
    private JCheckBox surfacePickableCB;

    /** Check Box for surface back face culling. */
    private JCheckBox surfaceBackFaceCB;

    /** Check Box for surface clpping of the volume render. */
    private JCheckBox surfaceClipCB;

    /** Check Box for surface transparency. */
    private JCheckBox surfaceTransparencyCB;

    /** The number of triangles label. */
    private JLabel triangleLabel;

    /** Displays the number of triangles. */
    private JTextField triangleText;

    /** The volume label. */
    private JLabel volumeLabel;

    /** Displays the volume of triangle. */
    private JTextField volumeText;

    /** Polyline counter list <index, groupID> */
    private DefaultListModel polylineCounterList = new DefaultListModel();
    
    /** constant polyline counter */
    private int polylineCounter = 0;
    
    /** Decimation Percentage */
    private double decimationPercentage = 95.0;
    
    /** triangle mesh for decimation. */
    private TriangleMesh[] tmesh;
    
    private Vector<SurfaceState> m_akSurfaceStates = new Vector<SurfaceState>();
    
    
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelSurface_WM( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        m_kSurfacePaint = new SurfacePaint_WM(this, m_kVolumeViewer);
        
		fixedColor = new ColorRGB[255];
		ModelLUT lut = new ModelLUT(ModelLUT.STRIPED, 256, new int[] {4, 256});	
		Color lutColor;
		for (int n=0;n<255;n++) 
		{
			lutColor = lut.getColor(n+1);
			fixedColor[n] = new ColorRGB( lutColor.getRed()/255f,
					lutColor.getGreen()/255f, lutColor.getBlue()/255f );
		}
        init();
    }
    
    /**
     * static function returns the next default surface color, based on the current number of surfaces displayed. If the
     * number of surfaces is less than the fixedColor.length then fixedColor is the source of the surface color,
     * otherwise a random color is generated.
     *
     * @param   index  the number of the new surface
     *
     * @return  Color4f, the default surface color for the new surface.
     */
    public static ColorRGB getNewSurfaceColor(int index) {
        ColorRGB surfaceColor = new ColorRGB();

        if (index < fixedColor.length) {
            // Use the fixed colors for the first six surfaces.
            surfaceColor.Copy( fixedColor[index] );
        }
        else
        {
            Random randomGen = new Random();

            // Use randomly generated colors for the seventh and
            // later surfaces.
            surfaceColor.Set( 0.5f * (1.0f + randomGen.nextFloat()),
                                  0.5f * (1.0f + randomGen.nextFloat()),
                                  0.5f * (1.0f + randomGen.nextFloat()) );
        }

        return surfaceColor;
    }

    /**
     * The override necessary to be an ActionListener. This callback is executed whenever the Add or Remove buttons are
     * clicked, or when the color button or light button is clicked, or when the combo box changes. If the Add button is
     * clicked, a file dialog is launched to allow the user to select new surface meshes to load from disk. If the
     * Remove button is clicked, the currently selected surfaces in the list box are removed from the scene graph.
     *
     * @param  event  The action event.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Add")) {
        	try {
            addSurface(); } catch ( NullPointerException e ) {}
        } 
        else if (command.equals("Remove")) {
            removeSurface();
        }
        else if (command.equals("AddPolyline")) {
            addPolyline();
        } 
        else if (command.equals("RemovePolyline")) {
            removePolyline();
        }
        else if (command.equals("ChangeColor")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(colorButton),
                                                 new CancelListener());
        } /* 
         else if (command.equals("ImageAsTexture")) {
            displayImageAsTexture(getSelectedSurfaces(surfaceList.getSelectedIndices()));
        } */ 
        else if (command.equals("AdvancedMaterialOptions")) {
            displayAdvancedMaterialOptions(surfaceList.getSelectedIndices());
        } 
        else if (command.equals("SurfaceTexture")) {
            m_kVolumeViewer.actionPerformed(new ActionEvent(m_kSurfaceTextureButton, 0, "SurfaceTexture"));
        }

        else if (command.equals("SurfacePickable")) {
            setPickable(surfaceList.getSelectedIndices());
        }  
        else if (command.equals("backface")) {
            setBackface(surfaceList.getSelectedIndices());
        }
        else if (command.equals("transparency")) {
            setTransparency(surfaceList.getSelectedIndices());
        }
        else if (command.equals("Clipping") )
        {
            setClipping(surfaceList.getSelectedIndices());
        }
        else if (command.equals("ChangePolyMode")) {
            changePolyMode(polygonIndexToMode(polygonModeCB.getSelectedIndex()));
        } 
        else if (command.equals("saveSurface")) {
            saveSurfaces( surfaceList.getSelectedIndices(), command );        	
        }
        else if (command.equals("savePLYSurface")) {
            saveProstateSurfaces( surfaceList.getSelectedIndices(), command );        	
        }
        else if (command.equals("saveSTLSurface")) {
            saveProstateSurfaces( surfaceList.getSelectedIndices(), command );        	
        }
        else if (command.equals("Smooth")) {
            smoothSurface(surfaceList.getSelectedIndices(), JDialogSmoothMesh.SMOOTH1);
        } else if (command.equals("Smooth2")) {
            smoothSurface(surfaceList.getSelectedIndices(), JDialogSmoothMesh.SMOOTH2);
        } else if (command.equals("Smooth3")) {
            smoothSurface(surfaceList.getSelectedIndices(), JDialogSmoothMesh.SMOOTH3);
        } 
        else if (command.equals("Decimate")) {
            decimate(surfaceList.getSelectedIndices());
        }
        else if ( command.equals("InvertNormals") )
        {
        	invertNormals(surfaceList.getSelectedIndices());
        }
        else if ( command.equals("ExtractConnected") )
        {
        	extractConnectedComponents(surfaceList.getSelectedIndices());
        }
        else if ( command.equals("ConvexHull") )
        {
        	convexHull(surfaceList.getSelectedIndices());
        }
        else if ( command.equals( "SubdivideTriangles" ) )
        {
        	subDivideTriangles( surfaceList.getSelectedIndices() );
        }
    }

    /**
     * Add surface to the volume image. Calls the FileSurface.openSurfaces function to open a file dialog so the user
     * can choose the surfaces to add.
     */
    public void addSurface() {
        TriMesh[] akSurfaces = FileSurface_WM.openSurfaces(m_kVolumeViewer.getImageA());
        
        if ( akSurfaces != null )
        {
            addSurfaces(akSurfaces);
        }
        
    }

    public Vector<SurfaceState> getSurfaceStates()
    {
        return m_akSurfaceStates;
    }

    public void setSurfaceStates(Vector<SurfaceState> kSurfaces)
    {
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        int iSize = kList.getSize();
        m_akSurfaceStates = kSurfaces;
        for ( int i = 0; i < kSurfaces.size(); i++ )
        {            
            kList.add( iSize + i, m_akSurfaceStates.get(i).Name );
            m_kVolumeViewer.addSurface( m_akSurfaceStates.get(i) );
            updateSelected(i);
        }
        surfaceList.setSelectedIndex(kSurfaces.size());
        setElementsEnabled(true);
    }
    
    
    /**
     * Add surfaces to the Volume Tri-Planar renderer.
     * @param akSurfaces new surfaces.
     */
    public void addSurfaces( TriMesh[] akSurfaces )
    {
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        int iSize = kList.getSize();
        for ( int i = 0; i < akSurfaces.length; i++ )
        {
        	kList.add( iSize + i, akSurfaces[i].GetName() );
        	SurfaceState kSurface = new SurfaceState( akSurfaces[i], akSurfaces[i].GetName() );
        	m_akSurfaceStates.add( kSurface );        
        	m_kVolumeViewer.addSurface( kSurface );
        }
        surfaceList.setSelectedIndex(iSize);
        setElementsEnabled(true);
    }

    /**
     * Changes the polygon mode of the selected surface by detaching it, calling the appropriate method, and reattaching
     * it.
     *
     * @param  mode  The new polygon mode to set.
     */
    public void changePolyMode(WireframeState.FillMode mode) {
        int[] aiSelected = surfaceList.getSelectedIndices();

        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_akSurfaceStates.get( aiSelected[i] ).Fill = mode;
                m_kVolumeViewer.setPolygonMode( (String)kList.elementAt(aiSelected[i]), mode);
            }
        }
    }

    /**
     * Dispose the local memory.
     */
    public void disposeLocal() {
    	int i;
    	
    	if ( tmesh != null ) {
    		for ( i = 0; i < tmesh.length; i++ ) {
    			tmesh[i].dispose();
    			tmesh[i] = null;
    		}
    		tmesh = null;
    	}
    	/*
    	if (  m_kMeshes != null ) {
    		for ( i = 0; i < m_kMeshes.size(); i++ ) {
    			m_kMeshes.set(i, null);
    		}
    		m_kMeshes = null;
    	} */
    	
    	if ( m_kSurfacePaint != null ) {
    		m_kSurfacePaint.dispose();
    		m_kSurfacePaint = null;
    	}
    	
    }

    /**
     * Enables/Disables the SurfacePaint per-vertex functions.
     * @param  bEnable  when true the SurfacePaint per-vertex functions (PaintBrush, Dropper, Eraser, BrushSize) are
     *                  enabled, when false they are disabled.
     */
    public void enableSurfacePaint(boolean bEnable) {
        m_kSurfacePaint.enableSurfacePaint(bEnable);
    }


    /**
     * Enables/Disables the SurfacePaint Paint Can function.
     * @param  bEnable  when true the Paint Can function is enabled, when false it is disabled.
     */
    public void enableSurfacePaintCan(boolean bEnable) {
        m_kSurfacePaint.enableSurfacePaintCan(bEnable);
    }

    /**
     * Return the name of the selected surface.
     * @return name of the selected surface.
     */
    public String getSelectedSurface()
    {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( aiSelected.length == 0 )
        {
            return null;
        }               
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        return (String)kList.elementAt(aiSelected[aiSelected.length - 1]);
    }

    /**
     * Return the names of the selected surfaces.
     * @return names of the selected surfaces.
     */
    public String[] getSelectedSurfaces() {
        int[] aiSelected = surfaceList.getSelectedIndices();
        String[] akNames = new String[aiSelected.length];
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        for (int i = 0; i < aiSelected.length; i++) {
            akNames[i] = new String((String)kList.elementAt(aiSelected[i]));
        }
        return akNames;
    }


    /**
     * Turn surface texture on/off.
     * @param bTextureOn texture on/off.
     * @param bUseNewImage when true use the user-specified ModelImage, when false use default ModelImage.
     * @param bUseNewLUT when true use the user-specified LUT, when false use the defaulet LUT.
     */
    public void ImageAsTexture( boolean bTextureOn, boolean bUseNewImage, boolean bUseNewLUT )
    {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.setSurfaceTexture( (String)kList.elementAt(aiSelected[i]),
                        bTextureOn, bUseNewImage, bUseNewLUT);
            }
        }
        enableSurfacePaintCan(bTextureOn);
    }

    /**
     * Check if the surface pickable checkbox be selected or not.
     * @return  isSelected Surface pickable check box selected or not.
     */
    public boolean isSurfacePickableSelected() {
        return surfacePickableCB.isSelected();
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        frameHeight = frameHeight - (40 * 2);
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
     */
    public void setButtonColor(JButton _button, Color _color) {

        super.setButtonColor(_button, _color);

        if ( m_kVolumeViewer != null )
        {
            int[] aiSelected = surfaceList.getSelectedIndices();
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                ColorRGB kColor = new ColorRGB( _color.getRed()/255.0f, 
                        _color.getGreen()/255.0f,
                        _color.getBlue()/255.0f );
                m_akSurfaceStates.get(aiSelected[i]).SurfaceColor = 
                    new Color( _color.getRed(), _color.getGreen(), _color.getBlue() );
                m_kVolumeViewer.setColor( (String)kList.elementAt(aiSelected[i]),
                        kColor, true );
            }
        }
    }

    /**
     * Set the paint can color.
     * @param kDropperColor color.
     * @param kPickPoint picked point on the surface.
     */
    public void setDropperColor( ColorRGBA kDropperColor, Vector3f kPickPoint )
    {
        if ( m_kSurfacePaint != null )
        {
            m_kSurfacePaint.setDropperColor(kDropperColor, kPickPoint);
        }
    }

    /**
     * Set the user-specified ModelImage to use as the surface texture.
     * @param kImage ModelImage to use as the surface texture.
     */
    public void SetImageNew(  ModelImage kImage )
    {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.SetImageNew( (String)kList.elementAt(aiSelected[i]), kImage);
            }
        }
    }

    /**
     * Set the user-specified LUT for surface texture.
     * @param kLUT ModelLUT
     * @param kRGBT ModelRGB for color images.
     */
    public void SetLUTNew(  ModelStorageBase kLUT )
    {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.SetLUTNew( (String)kList.elementAt(aiSelected[i]), kLUT);
            }
        }
    }

    /**
     * Called from the JPanelSurfaceMAterialProperties.java dialog when the dialog is used to change the material
     * properties of a surface. The surface is determined by the index iIndex. The color button is set to the Material
     * diffuse color.
     *
     * @param  kMaterial  Material reference
     * @param  iIndex     int material index
     */
    public void setMaterial(MaterialState kMaterial, int iIndex) {
        colorButton.setBackground( new Color(kMaterial.Diffuse.R, kMaterial.Diffuse.G, kMaterial.Diffuse.B) );
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            m_akSurfaceStates.get(iIndex).Material = kMaterial;
            m_kVolumeViewer.setMaterial( (String)kList.elementAt(iIndex), kMaterial, true);
        }
    }

    /* (non-Javadoc)
     * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
     */
    public void stateChanged(ChangeEvent event)
    {
        if (event.getSource() == opacitySlider)
        {
            setTransparency(surfaceList.getSelectedIndices());
        }

        if (event.getSource() == detailSlider) {

            if (detailSlider.getValueIsAdjusting()) {
                // Too many LOD changes occur if you always get the slider
                // value.  Wait until the user stops dragging the slider.

                // Maybe not. Comment out the next line to have the surface update quickly.
                // If the CLOD mesh holds a surface over time one could view the surface evolution !!!!
                return;
            }

            
            
            // value in [0,100], corresponds to percent of maximum LOD
            float fValue = 1.0f - (float)detailSlider.getValue() / (float)detailSlider.getMaximum();
            decimationPercentage = fValue * 100.0;
            
            int numTriangles = 0;
            // construct the lists of items whose LODs need to be changed
            int[] aiSelected = surfaceList.getSelectedIndices();
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            boolean found = false;
            if ( aiSelected.length == 0) return;
            
            for (int i = 0; i < aiSelected.length; i++) {

                if ( m_akSurfaceStates.get(aiSelected[i]).Surface instanceof ClodMesh )
                {
                    ClodMesh kCMesh = (ClodMesh)m_akSurfaceStates.get(aiSelected[i]).Surface;
                    int iValue = (int)(fValue * kCMesh.GetMaximumLOD());
                    kCMesh.TargetRecord( iValue );
                    kCMesh.SelectLevelOfDetail();
                    numTriangles += kCMesh.GetTriangleQuantity();
                    triangleText.setText("" + numTriangles);
                }
                else 
                {
                    TriMesh kMesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                    
                    try {
                    	 for ( int j = 0; j < tmesh.length; j++ ) {
                    		 
		                     if ( tmesh[j].GetName().equals(kMesh.GetName())) {
		                    	 found = true;
			                   	 tmesh[j].doDecimation(decimationPercentage);
			                   	 TriMesh mesh = new TriMesh(tmesh[j].getDecimatedVBuffer(), tmesh[j].getDecimatedIBuffer());
			                   	 mesh.SetName(new String(kMesh.GetName()));
			            		 SurfaceState kState = new SurfaceState( mesh, mesh.GetName() );
			            		 
			                     m_kVolumeViewer.removeSurface( (String)kList.elementAt(aiSelected[i]) );
			                     m_akSurfaceStates.remove(aiSelected[i]);
			                     
			                     m_akSurfaceStates.add( kState );        
			                     m_kVolumeViewer.addSurface(kState);
			
			                     numTriangles = tmesh[j].getDecimatedIBuffer().GetIndexQuantity();
		                     } 
                    	 }
                   	} catch ( Exception e ) {
                   		e.printStackTrace();
                   	}	
                    
                    
                   
                }
            }
            if ( found ) {
	            numTriangles /= 3;
	            triangleText.setText("" + numTriangles);
	            // keep the current selected mesh type: fill, points, or lines. 
	            changePolyMode(polygonIndexToMode(polygonModeCB.getSelectedIndex()));
            }
            
        }
    }
    
    /**
     * Returns true if a surface exists in the Renderer.
     * @return true if a surface exists in the Renderer.
     */
    public boolean surfaceAdded()
    {
        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        if ( kList.size() > 0 )
        {
            return true;
        }
        return false;
    }
    
    /**
     * Toggle which type of Geodesic is displayed on the surface (Euclidian, Dijkstra, Geodesic).
     * @param iWhich type of Geodesic is displayed on the surface (Euclidian, Dijkstra, Geodesic).
     */
    public void toggleGeodesicPathDisplay(int iWhich) {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.toggleGeodesicPathDisplay( (String)kList.elementAt(aiSelected[i]),
                        iWhich);
            }
        }
    }    
    /* (non-Javadoc)
     * @see javax.swing.event.ListSelectionListener#valueChanged(javax.swing.event.ListSelectionEvent)
     */
    public void valueChanged(ListSelectionEvent kEvent)
    {
        if ( m_kVolumeViewer != null )
        {
            int[] aiSelected = surfaceList.getSelectedIndices();
            if ( aiSelected.length == 1 )
            {
                setSelected( aiSelected[0] );
            }
        }
    }
    
    public int getSelected()
    {
        int[] aiSelected = surfaceList.getSelectedIndices();
        if ( aiSelected.length > 0 )
        {
            return aiSelected[0];
        }
        return 0;
    }
    
    public void setSelected( int i )
    {
        if ( m_akSurfaceStates.size() <= i )
        {
            return;
        }
        surfaceList.setSelectedIndex(i);
        SurfaceState kState = m_akSurfaceStates.get(i);
        triangleText.setText(String.valueOf( kState.Surface.GetTriangleQuantity() ) );
        volumeText.setText(String.valueOf( m_kVolumeViewer.getVolumeString( kState.Name ) ) );
        areaText.setText(String.valueOf( m_kVolumeViewer.getSurfaceAreaString( kState.Name ) ) );  

        colorButton.setBackground( kState.SurfaceColor );
        
        opacitySlider.setValue((int)(kState.Opacity * 100));
        surfaceTransparencyCB.setSelected( kState.TransparencyOn );
        
        polygonModeCB.setSelectedIndex( fillModeToPolygonIndex( kState.Fill ) );
        
        surfacePickableCB.setSelected( kState.Pickable );
        
        surfaceClipCB.setSelected( kState.Clip );
        
        surfaceBackFaceCB.setSelected( kState.BackfaceCull );

        if ( kState.Surface instanceof ClodMesh )
        {
            decimateButton.setEnabled(false);
            detailSlider.setEnabled(true);
            detailLabel.setEnabled(true);
        }
        else
        {
            decimateButton.setEnabled(true);
            detailSlider.setEnabled(false);
            detailLabel.setEnabled(false);                    
        }
    }
    
    private void updateSelected( int i )
    {
        SurfaceState kState = m_akSurfaceStates.get(i); 

        colorButton.setBackground( kState.SurfaceColor );
        m_kVolumeViewer.setColor( kState.Name, 
                new ColorRGB( kState.SurfaceColor.getRed()/255.0f,
                        kState.SurfaceColor.getGreen()/255.0f,
                        kState.SurfaceColor.getBlue()/255.0f ), false );
        m_kVolumeViewer.setMaterial( kState.Name, kState.Material, false );
        
        m_kVolumeViewer.setTransparency( kState.Name, kState.Opacity );
        
        m_kVolumeViewer.setPolygonMode( kState.Name, kState.Fill );
        
        m_kVolumeViewer.setPickable( kState.Name, kState.Pickable );
        
        m_kVolumeViewer.setClipping( kState.Name, kState.Clip );
        
        m_kVolumeViewer.setBackface( kState.Name, kState.BackfaceCull );
    }

    /** 
     * Add polyline to the render.
     */
    public void addPolyline() {
    	Polyline[] akPolylines = FilePolyline_WM.openPolylines(m_kVolumeViewer.getImageA());
    	Vector3f m_kTranslate = m_kVolumeViewer.getVolumeGPU().getTranslate();
    	
    	 float m_fMax, m_fX, m_fY, m_fZ;
    	 float fMaxX = (m_kVolumeViewer.getImageA().getExtents()[0] - 1) * m_kVolumeViewer.getImageA().getFileInfo(0).getResolutions()[0];
         float fMaxY = (m_kVolumeViewer.getImageA().getExtents()[1] - 1) * m_kVolumeViewer.getImageA().getFileInfo(0).getResolutions()[1];
         float fMaxZ = (m_kVolumeViewer.getImageA().getExtents()[2] - 1) * m_kVolumeViewer.getImageA().getFileInfo(0).getResolutions()[2];

         m_fMax = fMaxX;
         if (fMaxY > m_fMax) {
             m_fMax = fMaxY;
         }
         if (fMaxZ > m_fMax) {
             m_fMax = fMaxZ;
         }
         m_fX = fMaxX/m_fMax;
         m_fY = fMaxY/m_fMax;
         m_fZ = fMaxZ/m_fMax;
    	
    	if ( akPolylines != null )
        {
        	DefaultListModel kList = (DefaultListModel)polylineList.getModel();
            int iSize = kList.getSize();
            
            for ( int i = 0; i < akPolylines.length ; i++ ) {
            	polylineCounterList.add(iSize + i, polylineCounter);
            	
            	 
            	 for ( int j = 0; j < akPolylines[i].VBuffer.GetVertexQuantity(); j++ )
                 {

           		  akPolylines[i].VBuffer.SetPosition3(j, akPolylines[i].VBuffer.GetPosition3fX(j) - m_kTranslate.X,
           				  akPolylines[i].VBuffer.GetPosition3fY(j) - m_kTranslate.Y, 
           				  akPolylines[i].VBuffer.GetPosition3fZ(j) - m_kTranslate.Z );
           		   akPolylines[i].VBuffer.SetPosition3(j, 
           				akPolylines[i].VBuffer.GetPosition3fX(j) * 1.0f/m_fX,
           				akPolylines[i].VBuffer.GetPosition3fY(j) * 1.0f/m_fY,
           				akPolylines[i].VBuffer.GetPosition3fZ(j) * 1.0f/m_fZ);
                 } 
                
                akPolylines[i].Local.SetTranslate(new Vector3f(m_kTranslate.X, m_kTranslate.Y, m_kTranslate.Z));
            	m_kVolumeViewer.addPolyline(null, akPolylines[i], polylineCounter);
            	
            	polylineCounter++;
            }
            
            for ( int i = 0; i < akPolylines.length; i++ )
            {
                kList.add( iSize + i, akPolylines[i].GetName() );
               
            }
            polylineList.setSelectedIndex(iSize);
            
        }
    }
    
    /**
     * Build the toolbar.
     */
    private void buildToolBar() {

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);

        JToolBar toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setFloatable(false);
        
        subdivideTriangles = toolbarBuilder.buildButton("SubdivideTriangles", "divide triangles", "decimate");
        convexHull = toolbarBuilder.buildButton("ConvexHull", "compute convex hull", "decimate");
        extractConnected = toolbarBuilder.buildButton("ExtractConnected", "Extract connected components", "decimate");
        invertNormals = toolbarBuilder.buildButton("InvertNormals", "Reverses triangle order", "decimate");
        smooth1Button = toolbarBuilder.buildButton("Smooth", "Smooth level 1", "sm1");
        smooth2Button = toolbarBuilder.buildButton("Smooth2", "Smooth level 2", "sm2");
        smooth3Button = toolbarBuilder.buildButton("Smooth3", "Smooth level 3", "sm3");
        decimateButton = toolbarBuilder.buildButton("Decimate", "Decimate the surface", "decimate");
        saveSurfaceButton = toolbarBuilder.buildButton("saveSurface", "Save surface to a file", "save");
        savePLYSurfaceButton = toolbarBuilder.buildButton("savePLYSurface", "Save prostate surface to a PLY file", "saveply");
        saveSTLSurfaceButton = toolbarBuilder.buildButton("saveSTLSurface", "Save prostate surface to a STL file", "savestl");
        
        toolBar.add(invertNormals);
        toolBar.add(subdivideTriangles);
        toolBar.add(convexHull);
        toolBar.add(extractConnected);
        toolBar.add(smooth1Button);
        toolBar.add(smooth2Button);
        toolBar.add(smooth3Button);
        toolBar.add(decimateButton);
        toolBar.add(saveSurfaceButton);
        toolBar.add(savePLYSurfaceButton);
        toolBar.add(saveSTLSurfaceButton);

        JPanel toolBarPanel = new JPanel();
        toolBarPanel.setLayout(new BorderLayout());
        toolBarPanel.add(toolBar, BorderLayout.WEST);
        toolBarPanel.add(m_kSurfacePaint.getToolBar(), BorderLayout.SOUTH);

        mainPanel.add(toolBarPanel, BorderLayout.NORTH);
    }
    
    /**
     * Creates a label in the proper font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    private JLabel createLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
    }

    /**
     * Decimate the selected surfaces.
     * @param  aiSelected   selected surfaces.
     */
    private void decimate(int[] aiSelected) {
        
        if ( m_kVolumeViewer != null )
        {
            //TriMesh[] akSurfaces = new TriMesh[ aiSelected.length ];
           
            //DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            tmesh = new TriangleMesh[ aiSelected.length ];
            for (int i = 0; i < aiSelected.length; i++) {
            	
                TriMesh kMesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                
                VertexBuffer kVBuffer = new VertexBuffer(kMesh.VBuffer);
                IndexBuffer kIBuffer = new IndexBuffer( kMesh.IBuffer);
                tmesh[i] = new TriangleMesh(kVBuffer, kIBuffer);
                tmesh[i].SetName(kMesh.GetName());
                
                //TriMesh mesh = new TriMesh(kVBuffer, kIBuffer);
                //mesh.SetName( kMesh.GetName() );
                //akSurfaces[i] = mesh;

                //m_kVolumeViewer.removeSurface( (String)kList.elementAt(aiSelected[i]) );
                //m_akSurfaceStates.get(aiSelected[0]).Surface = mesh;
                //m_kVolumeViewer.addSurface(m_akSurfaceStates.get(aiSelected[0]));
            }




            decimateButton.setEnabled(false);
            detailSlider.setEnabled(true);
            detailLabel.setEnabled(true);

            for (int j = 0; j < detailSliderLabels.length; j++) {
                detailSliderLabels[j].setEnabled(true);
            }
        }
    }

    /**
     * Decimate the selected surfaces.
     * @param  aiSelected   selected surfaces.
     */
    private void invertNormals(int[] aiSelected) {
        
        if ( m_kVolumeViewer != null )
        {           
            for (int i = 0; i < aiSelected.length; i++) {
            	
                TriMesh kMesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                IndexBuffer kIBuffer = kMesh.IBuffer;
                int[] aiData = kIBuffer.GetData();
                for ( int j = 0; j < (aiData.length - 2); j += 3 )
                {
                	int temp = aiData[j+1];
                	aiData[j+1] = aiData[j+2];
                	aiData[j+2] = temp;
                }
                kMesh.UpdateMS(true);
                kMesh.Reload(true);
            }
        }
    }

    /**
     * Decimate the selected surfaces.
     * @param  aiSelected   selected surfaces.
     */
    private void extractConnectedComponents(int[] aiSelected) {
        
        if ( m_kVolumeViewer != null )
        {           
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            int iSize = kList.getSize();
            for (int i = 0; i < aiSelected.length; i++) {
            	
                TriMesh kMesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                VETMesh kVETMesh = new VETMesh(2 * kMesh.VBuffer.GetVertexQuantity(), .9f, 2 * kMesh.IBuffer
                        .GetIndexQuantity(), .9f, 2 * kMesh.GetTriangleQuantity(), .9f, kMesh.IBuffer.GetData());

                final Vector<VETMesh> kComponents = new Vector<VETMesh>();
                kVETMesh.GetComponents(kComponents);
                
                if ( kComponents.size() > 1 )
                {
                	//System.err.println( kComponents.size() );
                	int largest = 0;
                	int max = 0;
                	for ( int j = 0; j < kComponents.size(); j++ )
                	{
                		if ( kComponents.elementAt(j).GetTriangleQuantity() > max )
                		{
                			max = kComponents.elementAt(j).GetTriangleQuantity();
                			largest = j;
                		}
                	}
                	VertexBuffer kVBuffer = new VertexBuffer(kMesh.VBuffer);
                	IndexBuffer kIBuffer = new IndexBuffer( kComponents.elementAt(largest).GetTriangles() );
                	TriMesh mesh = new TriMesh(kVBuffer, kIBuffer);
                	mesh = removeUnusedVertices( mesh );
                	mesh.SetName(kMesh.GetName() + "_" + largest );

                	SurfaceState kSurface = new SurfaceState( mesh, mesh.GetName() );

                	//System.err.println( mesh.GetName() );

                	kList.add( iSize++, mesh.GetName() );

                	m_akSurfaceStates.add( kSurface );        
                	m_kVolumeViewer.addSurface( kSurface );
                }
            }
        }
    }
    
    private void convexHull(int[] aiSelected) {
        
        if ( m_kVolumeViewer != null )
        {           
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            int iSize = kList.getSize();
            for (int i = 0; i < aiSelected.length; i++) {
            	
                TriMesh mesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                TriMesh chMesh = convexHull(mesh);
                
                chMesh.SetName(mesh.GetName() + "_convex_hull" );

            	SurfaceState kSurface = new SurfaceState( chMesh, chMesh.GetName() );

            	//System.err.println( mesh.GetName() );

            	kList.add( iSize++, chMesh.GetName() );

            	m_akSurfaceStates.add( kSurface );        
            	m_kVolumeViewer.addSurface( kSurface );
            }
        }
    }

    
    private float[] CalcMinMaxEdgeLength( TriMesh mesh )
    {
    	float[] distance = new float[]{Float.MAX_VALUE, -Float.MAX_VALUE};
    
    	int[] tris = new int[3];
    	for ( int i = 0; i < mesh.GetTriangleQuantity(); i++ )
    	{
    		if ( mesh.GetTriangle(i, tris) )
    		{
    			Vector3f p0 = mesh.VBuffer.GetPosition3(tris[0]);
    			Vector3f p1 = mesh.VBuffer.GetPosition3(tris[1]);
    			Vector3f p2 = mesh.VBuffer.GetPosition3(tris[2]);

    			float lengthP0P1 = p0.distance(p1);
    			float lengthP1P2 = p1.distance(p2);
    			float lengthP2P0 = p2.distance(p0);

    			distance[0] = Math.min( distance[0], lengthP0P1 );
    			distance[0] = Math.min( distance[0], lengthP1P2 );
    			distance[0] = Math.min( distance[0], lengthP2P0 );

    			distance[1] = Math.max( distance[1], lengthP0P1 );
    			distance[1] = Math.max( distance[1], lengthP1P2 );
    			distance[1] = Math.max( distance[1], lengthP2P0 );
    		}
    	}
    	
    	return distance;
    }
    
    private void subDivideTriangles( int[] aiSelected )
    {
        if ( m_kVolumeViewer != null )
        {           
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            int iSize = kList.getSize();
            for (int i = 0; i < aiSelected.length; i++) {
            	
                TriMesh mesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                
                float[] distance = CalcMinMaxEdgeLength(mesh);
                System.err.println( distance[0] + " " + distance[1] );
                
                TriMesh chMesh = StandardMesh.SubDivide(mesh, distance[1]/4f );
                
                chMesh.SetName(mesh.GetName() + "_div" );

            	SurfaceState kSurface = new SurfaceState( chMesh, chMesh.GetName() );

            	//System.err.println( mesh.GetName() );

            	kList.add( iSize++, chMesh.GetName() );

            	m_akSurfaceStates.add( kSurface );        
            	m_kVolumeViewer.addSurface( kSurface );
            }
        }
    }
    
    

    private TriMesh convexHull( TriMesh mesh )
    {
		ConvexHull3f convexHull = new ConvexHull3f( mesh.VBuffer.GetVertexQuantity(), mesh.VBuffer.GetPositionArray(), 0.00001f, true );
		IndexBuffer iBuffer = new IndexBuffer(convexHull.GetIndices());
		VertexBuffer vBuffer = new VertexBuffer( mesh.VBuffer );
		TriMesh convexHullMesh = new TriMesh( vBuffer, iBuffer );

		System.err.println( mesh.GetTriangleQuantity() + " " + convexHullMesh.GetTriangleQuantity() );
//		System.err.println( convexHullMesh.VBuffer.GetVertexQuantity() );
		convexHullMesh = removeUnusedVertices(convexHullMesh);
		return convexHullMesh;
    }

    

    /**
     * Display the Surface Material dialog for the selected surfaces.
     * @param aiSelected the selected surfaces.
     */
    private void displayAdvancedMaterialOptions(int[] aiSelected) {
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                new JFrameSurfaceMaterialProperties_WM(this, aiSelected[i], m_kVolumeViewer.GetLights(),
                        m_kVolumeViewer.getMaterial( (String)kList.elementAt(aiSelected[i]) ) );
            }
        }
    }
   
    /**
     * Initializes the GUI components.
     */
    private void init() {
        // setSize(400, 256);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());
        buildToolBar();

        // Layout
        //
        // +-----------------------------------+
        // |                       color button|
        // |   surfaceList         opacity     |
        // |                       shininess   |
        // |                       triangles   |
        // |                       LOD slider  |
        // |   add  remove         light button|
        // +-----------------------------------+

        JPanel buttonPanel = new JPanel();

        // buttons for add/remove of surfaces from list
        JButton addButton = new JButton("Add");
        addButton.addActionListener(this);
        addButton.setActionCommand("Add");
        addButton.setFont(MipavUtil.font12B);
        addButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton removeButton = new JButton("Remove");
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");
        removeButton.setFont(MipavUtil.font12B);
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        
        buttonPanel.add(addButton);
        buttonPanel.add(removeButton);
        
        // list panel for surface filenames
        surfaceList = new JList(new DefaultListModel());
        surfaceList.addListSelectionListener(this);
        surfaceList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPane = new JScrollPane(surfaceList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanel = new JPanel();
        listPanel.setLayout(new BorderLayout());
        listPanel.add(scrollPanel, BorderLayout.CENTER);
        listPanel.add(buttonPanel, BorderLayout.SOUTH);
        listPanel.setBorder(buildTitledBorder("Surface list"));

        // Polyline start
        JPanel buttonPanelPolyline = new JPanel();

        // buttons for add/remove of surfaces from list
        JButton addButtonPolyline = new JButton("Add");

        addButtonPolyline.addActionListener(this);
        addButtonPolyline.setActionCommand("AddPolyline");
        addButtonPolyline.setFont(MipavUtil.font12B);
        addButtonPolyline.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton removeButtonPolyline = new JButton("Remove");

        removeButtonPolyline.addActionListener(this);
        removeButtonPolyline.setActionCommand("RemovePolyline");
        removeButtonPolyline.setFont(MipavUtil.font12B);
        removeButtonPolyline.setPreferredSize(MipavUtil.defaultButtonSize);

        buttonPanelPolyline.add(addButtonPolyline);
        buttonPanelPolyline.add(removeButtonPolyline);

        // list panel for surface filenames
        polylineList = new JList(new DefaultListModel());
        polylineList.addListSelectionListener(this);
        polylineList.setPrototypeCellValue("aaaaaaaaaaaaaaaa.aaa    ");

        JScrollPane kScrollPanePolyline = new JScrollPane(polylineList);
        JPanel scrollPanelPolyline = new JPanel();

        scrollPanelPolyline.setLayout(new BorderLayout());
        scrollPanelPolyline.add(kScrollPanePolyline);
        scrollPanelPolyline.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel listPanelPolyline = new JPanel();
        listPanelPolyline.setLayout(new BorderLayout());
        listPanelPolyline.add(scrollPanelPolyline, BorderLayout.CENTER);
        listPanelPolyline.add(buttonPanelPolyline, BorderLayout.SOUTH);
        listPanelPolyline.setBorder(buildTitledBorder("Polyline list"));
        // polyline end
        
        JPanel paintTexturePanel = new JPanel();
        paintTexturePanel.setLayout(new FlowLayout(FlowLayout.LEFT));


        /* Creates the surface paint button, which launches the surface
         * dialog: */
        m_kSurfaceTextureButton = new JButton("Surface Texture");
        m_kSurfaceTextureButton.addActionListener(this);
        m_kSurfaceTextureButton.setActionCommand("SurfaceTexture");
        m_kSurfaceTextureButton.setSelected(false);
        m_kSurfaceTextureButton.setEnabled(false);
        paintTexturePanel.add(m_kSurfaceTextureButton);

        colorButton = new JButton("   ");
        colorButton.setToolTipText("Change surface color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("ChangeColor");

        colorLabel = new JLabel("Surface color");
        colorLabel.setFont(MipavUtil.font12B);
        colorLabel.setForeground(Color.black);

        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        colorPanel.add(colorButton);
        colorPanel.add(colorLabel);

        /* Creates the advanced material options button, which launches the
         * material editor dialog: */
        m_kAdvancedMaterialOptionsButton = new JButton("Advanced Options");
        m_kAdvancedMaterialOptionsButton.setToolTipText("Change surface material properties");
        m_kAdvancedMaterialOptionsButton.addActionListener(this);
        m_kAdvancedMaterialOptionsButton.setActionCommand("AdvancedMaterialOptions");
        m_kAdvancedMaterialOptionsButton.setEnabled(false);
        colorPanel.add(m_kAdvancedMaterialOptionsButton);

        // Slider for changing opacity; not currently enabled.
        opacityLabel = new JLabel("Opacity");
        opacityLabel.setFont(MipavUtil.font12B);
        opacityLabel.setForeground(Color.black);

        opacitySliderLabels = new JLabel[3];
        detailSliderLabels = new JLabel[3];
        opacitySliderLabels[0] = createLabel("0");
        opacitySliderLabels[1] = createLabel("50");
        opacitySliderLabels[2] = createLabel("100");
        detailSliderLabels[0] = createLabel("0");
        detailSliderLabels[1] = createLabel("50");
        detailSliderLabels[2] = createLabel("100");

        Hashtable<Integer,JLabel> labels = new Hashtable<Integer,JLabel>();

        labels.put(new Integer(0), opacitySliderLabels[0]);
        labels.put(new Integer(50), opacitySliderLabels[1]);
        labels.put(new Integer(100), opacitySliderLabels[2]);

        opacitySlider = new JSlider(0, 100, 100);
        opacitySlider.setFont(MipavUtil.font12);
        opacitySlider.setMinorTickSpacing(10);
        opacitySlider.setPaintTicks(true);
        opacitySlider.addChangeListener(this);
        opacitySlider.setLabelTable(labels);
        opacitySlider.setPaintLabels(true);
        opacitySlider.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacityLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacitySlider.setEnabled(true);
        opacityLabel.setEnabled(true);
        opacitySliderLabels[0].setEnabled(true);
        opacitySliderLabels[1].setEnabled(true);
        opacitySliderLabels[2].setEnabled(true);

        triangleLabel = new JLabel("Number of triangles");
        triangleLabel.setFont(MipavUtil.font12B);
        triangleLabel.setForeground(Color.black);
        triangleLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        triangleText = new JTextField(10);
        triangleText.setEditable(false);
        triangleText.setBorder(new EmptyBorder(triangleText.getInsets()));
        triangleText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel trianglePanel = new JPanel();

        trianglePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        trianglePanel.add(triangleLabel);
        trianglePanel.add(triangleText);
        trianglePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        volumeLabel = new JLabel("Volume of mesh");
        volumeLabel.setFont(MipavUtil.font12B);
        volumeLabel.setForeground(Color.black);
        volumeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        volumeText = new JTextField(20);
        volumeText.setEditable(false);
        volumeText.setBorder(new EmptyBorder(volumeText.getInsets()));
        volumeText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel volumePanel = new JPanel();

        volumePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        volumePanel.add(volumeLabel);
        volumePanel.add(volumeText);
        volumePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        areaLabel = new JLabel("Surface area");
        areaLabel.setFont(MipavUtil.font12B);
        areaLabel.setForeground(Color.black);
        areaLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        areaText = new JTextField(20);
        areaText.setEditable(false);
        areaText.setBorder(new EmptyBorder(areaText.getInsets()));
        areaText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel areaPanel = new JPanel();

        areaPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        areaPanel.add(areaLabel);
        areaPanel.add(areaText);
        areaPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Slider for changing level of detail.  Range is [0,100] with initial
        // value 100.
        detailLabel = new JLabel("Level of Detail");
        detailLabel.setFont(MipavUtil.font12B);
        detailLabel.setForeground(Color.black);

        Hashtable<Integer,JLabel> labels2 = new Hashtable<Integer,JLabel>();

        labels2.put(new Integer(0), detailSliderLabels[0]);
        labels2.put(new Integer(50), detailSliderLabels[1]);
        labels2.put(new Integer(100), detailSliderLabels[2]);

        detailSlider = new JSlider(0, 100, 100);
        detailSlider.setFont(MipavUtil.font12);
        detailSlider.setMinorTickSpacing(10);
        detailSlider.setPaintTicks(true);
        detailSlider.addChangeListener(this);
        detailSlider.setLabelTable(labels2);
        detailSlider.setPaintLabels(true);
        detailSlider.setAlignmentX(Component.LEFT_ALIGNMENT);
        detailLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        detailSlider.setEnabled(false);
        detailLabel.setEnabled(false);
        detailSliderLabels[0].setEnabled(false);
        detailSliderLabels[1].setEnabled(false);
        detailSliderLabels[2].setEnabled(false);

        JPanel sliderPanel = new JPanel();

        sliderPanel.setLayout(new BoxLayout(sliderPanel, BoxLayout.Y_AXIS));
        sliderPanel.add(opacityLabel);
        sliderPanel.add(opacitySlider);
        sliderPanel.add(trianglePanel);
        sliderPanel.add(volumePanel);
        sliderPanel.add(areaPanel);
        sliderPanel.add(detailLabel);
        sliderPanel.add(detailSlider);

        comboLabel = new JLabel("Polygon mode:");
        comboLabel.setFont(MipavUtil.font12B);
        comboLabel.setForeground(Color.black);

        polygonModeCB = new JComboBox(new String[] { "Fill", "Line", "Point" });
        polygonModeCB.addActionListener(this);
        polygonModeCB.setActionCommand("ChangePolyMode");
        polygonModeCB.setAlignmentX(Component.LEFT_ALIGNMENT);
        polygonModeCB.setFont(MipavUtil.font12);
        polygonModeCB.setBackground(Color.white);

        surfacePickableCB = new JCheckBox("Surface Pickable", true);
        surfacePickableCB.addActionListener(this);
        surfacePickableCB.setActionCommand("SurfacePickable");
        surfacePickableCB.setFont(MipavUtil.font12B);
        surfacePickableCB.setSelected(false);

        surfaceClipCB = new JCheckBox("Surface Clipping", true);
        surfaceClipCB.addActionListener(this);
        surfaceClipCB.setActionCommand("Clipping");
        surfaceClipCB.setFont(MipavUtil.font12B);
        surfaceClipCB.setSelected(false);
        surfaceClipCB.setEnabled(false);

        surfaceBackFaceCB = new JCheckBox("Backface Culling", true);
        surfaceBackFaceCB.addActionListener(this);
        surfaceBackFaceCB.setActionCommand("backface");
        surfaceBackFaceCB.setFont(MipavUtil.font12B);
        surfaceBackFaceCB.setSelected(true);

        surfaceTransparencyCB = new JCheckBox("Transparency", true);
        surfaceTransparencyCB.addActionListener(this);
        surfaceTransparencyCB.setActionCommand("transparency");
        surfaceTransparencyCB.setFont(MipavUtil.font12B);
        surfaceTransparencyCB.setSelected(true);
        
        JPanel cbSurfacePanel = new JPanel();
        cbSurfacePanel.setLayout(new BoxLayout(cbSurfacePanel, BoxLayout.Y_AXIS));
        cbSurfacePanel.add(surfacePickableCB);
        cbSurfacePanel.add(surfaceClipCB);
        cbSurfacePanel.add(surfaceBackFaceCB);
        cbSurfacePanel.add(surfaceTransparencyCB);

        JPanel cbPanel = new JPanel();
        cbPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        cbPanel.add(comboLabel);
        cbPanel.add(polygonModeCB);
        cbPanel.add(cbSurfacePanel);

        paintTexturePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        sliderPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        cbPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        colorPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        sliderPanel.setAlignmentY(Component.TOP_ALIGNMENT);
        cbPanel.setAlignmentY(Component.TOP_ALIGNMENT);

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
        optionsPanel.add(colorPanel);
        optionsPanel.add(paintTexturePanel);
        optionsPanel.add(sliderPanel);
        optionsPanel.add(cbPanel);
        optionsPanel.setBorder(buildTitledBorder("Surface options"));

        JPanel rightPanel = new JPanel();
        rightPanel.setLayout(new BorderLayout());
        rightPanel.add(optionsPanel, BorderLayout.NORTH);

        
        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.setTabLayoutPolicy(JTabbedPane.WRAP_TAB_LAYOUT);
        tabbedPane.addChangeListener(this);
        
        tabbedPane.addTab("Surface", null, listPanel);
        // tabbedPane.addTab("Polyline", null, listPanelPolyline);
        tabbedPane.setSelectedIndex(0);
     
        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(tabbedPane);
        contentBox.add(rightPanel);
 
        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        mainPanel.add(scroller, BorderLayout.CENTER);

        // no surfaces yet, so the elements shouldn't be enabled
        setElementsEnabled(false);
 
        
    }

    /**
     * Convert from the polygon mode combo-box list index to the PolygonAttributes.POLYGON_LINE,
     * PolygonAttributes.POLYGON_POINT, and PolygonAttributes.POLYGON_FILL values:
     *
     * @param   index  the index of the selected polygon mode in the polygonModeCB combo box.
     *
     * @return  the corresponding PolygonAttributes defined value.
     */
    private WireframeState.FillMode polygonIndexToMode(int index) {
        switch (index) {

            case 1:
                return WireframeState.FillMode.FM_LINE;

            case 2:
                return WireframeState.FillMode.FM_POINT;

            case 0:
            default:
                return WireframeState.FillMode.FM_FILL;
        }
    }
    
    private int fillModeToPolygonIndex(WireframeState.FillMode mode) {
        switch (mode) {
            case FM_LINE:
                return 1;
            case FM_POINT:
                return 2;
            case FM_FILL:
            default:
                return 0;
        }
    }

    /**
     * Remove polyline from the render
     */
    private void removePolyline() {
    	int[] aiSelected = polylineList.getSelectedIndices();
    	int index;
        DefaultListModel kList = (DefaultListModel)polylineList.getModel();
        if ( m_kVolumeViewer != null )
        {
            for (int i = 0; i < aiSelected.length; i++) {
            	index = (Integer)(polylineCounterList.elementAt(aiSelected[i]));
                m_kVolumeViewer.removePolyline( index );
                polylineCounterList.remove(aiSelected[i]);
            }
        }
        for (int i = 0; i < aiSelected.length; i++) {
            kList.remove(aiSelected[i]);
        }
    }

    /**
     * Remove the selected surfaces.
     */
    private void removeSurface()
    {
        int[] aiSelected = surfaceList.getSelectedIndices();

        DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
        if ( m_kVolumeViewer != null )
        {
            for (int i = 0; i < aiSelected.length; i++) {
                m_kVolumeViewer.removeSurface( (String)kList.elementAt(aiSelected[i]) );
            }
        }
        for (int i = 0; i < aiSelected.length; i++) {
            kList.remove(aiSelected[i]);
            m_akSurfaceStates.remove(aiSelected[i]);
        }
    }
    
    
    private TriMesh removeUnusedVertices( TriMesh kMesh )
    {
    	int[] aiIndex = kMesh.IBuffer.GetData();
    	Vector<Vector3f> vertexList = new Vector<Vector3f>();
    	Vector<Integer> triList = new Vector<Integer>();
    	int indexCount = 0;
    	for ( int i = 0; i < aiIndex.length; i++ )
    	{
    		Vector3f kPos = kMesh.VBuffer.GetPosition3(aiIndex[i]);
    		if ( !vertexList.contains( kPos ) )
    		{
    			vertexList.add( kPos );
    			triList.add( indexCount++ );
    		}
    		else
    		{
    			triList.add( vertexList.indexOf( kPos ) );
    		}
    	}
    	if ( vertexList.size() == kMesh.VBuffer.GetVertexQuantity() )
    	{
    		System.err.println( "All vertices used" );
    		return kMesh;
    	}
    	int[] aiNewIndex  = new int[ triList.size() ];
    	for ( int i = 0; i < triList.size(); i++ )
    	{
    		aiNewIndex[i] = triList.elementAt(i);
    	}
    	IndexBuffer kIBuffer = new IndexBuffer( aiNewIndex );
    	VertexBuffer kVBuffer = new VertexBuffer( vertexList );
    	return new TriMesh( kVBuffer, kIBuffer );
    }
    
    /**
     * Save the selected surfaces. The kCommand parameter determines the file type.
     * @param aiSelected selected surfaces.
     * @param kCommand save command, specifies the file type.
     */
    private void saveSurfaces( int[] aiSelected, String kCommand )
    {

        if (aiSelected == null) {
            MipavUtil.displayError("Select a surface to save.");
            return;
        }  
        if (aiSelected.length != 1) {
            MipavUtil.displayError("Select one surface to save.");
            return;
        }

        if ( m_kVolumeViewer != null )
        {
            TriMesh[] akSurfaces = new TriMesh[ aiSelected.length ];
            for (int i = 0; i < aiSelected.length; i++) {
                akSurfaces[i] = m_akSurfaceStates.get(aiSelected[i]).Surface;
            }

            File[] akFiles = FileSurface_WM.openFiles(false);

            if (akFiles == null) {
                return;
            }
            String kName = akFiles[0].getAbsolutePath();
                                             

            try {
                float[] startLocation = m_kVolumeViewer.getImageA().getFileInfo(0).getOrigin();
                int[] extents = m_kVolumeViewer.getImageA().getExtents();
                int xDim = extents[0];
                int yDim = extents[1];
                int zDim = extents[2];
                
                float[] resols = m_kVolumeViewer.getImageA().getFileInfo()[0].getResolutions();
                float xBox = (xDim - 1) * resols[0];
                float yBox = (yDim - 1) * resols[1];
                float zBox = (zDim - 1) * resols[2];
                float maxBox = Math.max(xBox, Math.max(yBox, zBox));

                int[] direction = MipavCoordinateSystems.getModelDirections(m_kVolumeViewer.getImageA());
                float[] box = new float[]{ xBox, yBox, zBox };

                //                 for (int i = 0; i < meshes.length; i++) {
                VertexBuffer kVBuffer = new VertexBuffer( akSurfaces[0].VBuffer );

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (int j = 0; j < kVBuffer.GetVertexQuantity(); j++) {
                	kVBuffer.SetPosition3( j,
                			((((akSurfaces[0].VBuffer.GetPosition3fX(j) * 2.0f * maxBox) + xBox) / 2.0f) * direction[0]) +
                			startLocation[0],
                			((((akSurfaces[0].VBuffer.GetPosition3fY(j) * 2.0f * maxBox) + yBox) / 2.0f) * direction[1]) +
                			startLocation[1],
                			((((akSurfaces[0].VBuffer.GetPosition3fZ(j) * 2.0f * maxBox) + zBox) / 2.0f) * direction[2]) +
                			startLocation[2] );

                }

                //double[][] inverseDicomArray = null;
                TransMatrix inverse_DicomMatrix = null;
                if (m_kVolumeViewer.getImageA().getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                	inverse_DicomMatrix = m_kVolumeViewer.getImageA().getMatrix().clone();
                	inverse_DicomMatrix.Inverse();
                	//inverseDicomArray = inverseDicomMatrix.getMatrix();
                }

                TriMesh surfaceSave = new TriMesh( kVBuffer, akSurfaces[0].IBuffer );
                //int iType = akSurfaces[0] instanceof ClodMesh ? 1 : 0;
                FileSurface_WM.save(kName, surfaceSave, 0, surfaceSave.VBuffer, true, direction, startLocation, box, inverse_DicomMatrix);
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }
    
    /**
     * Save the selected surfaces. The kCommand parameter determines the file type.
     * @param aiSelected selected surfaces.
     * @param kCommand save command, specifies the file type.
     */
    private void saveProstateSurfaces( int[] aiSelected, String kCommand )
    {

        if (aiSelected == null) {
            MipavUtil.displayError("Select a surface to save.");
            return;
        }  
        if (aiSelected.length != 1) {
            MipavUtil.displayError("Select one surface to save.");
            return;
        }

        if ( m_kVolumeViewer != null )
        {
            TriMesh[] akSurfaces = new TriMesh[ aiSelected.length ];
            for (int i = 0; i < aiSelected.length; i++) {
                akSurfaces[i] = m_akSurfaceStates.get(aiSelected[i]).Surface;
            }

            File[] akFiles = FileSurface_WM.openFiles(false);

            if (akFiles == null) {
                return;
            }
            String kName = akFiles[0].getAbsolutePath();
                                             

            try {
                float[] startLocation = m_kVolumeViewer.getImageA().getFileInfo(0).getOrigin();
                int[] extents = m_kVolumeViewer.getImageA().getExtents();
                int xDim = extents[0];
                int yDim = extents[1];
                int zDim = extents[2];
                
                float[] resols = m_kVolumeViewer.getImageA().getFileInfo()[0].getResolutions();
                float xBox = (xDim - 1) * resols[0];
                float yBox = (yDim - 1) * resols[1];
                float zBox = (zDim - 1) * resols[2];
                float maxBox = Math.max(xBox, Math.max(yBox, zBox));

                int[] direction = MipavCoordinateSystems.getModelDirections(m_kVolumeViewer.getImageA());
                float[] box = new float[]{ xBox, yBox, zBox };

                //                 for (int i = 0; i < meshes.length; i++) {
                VertexBuffer kVBuffer = new VertexBuffer( akSurfaces[0].VBuffer );

                // The loaded vertices go from -(xDim-1)*resX/maxBox to (xDim-1)*resX/maxBox
                // The loaded vertex is at 2.0f*pt.x*resX - (xDim-1)*resX
                // The mesh files must save the verticies as
                // pt.x*resX*direction[0] + startLocation
                for (int j = 0; j < kVBuffer.GetVertexQuantity(); j++) {
                	kVBuffer.SetPosition3( j,
                			((((akSurfaces[0].VBuffer.GetPosition3fX(j) * 2.0f * maxBox) + xBox) / 2.0f) * direction[0]) +
                			startLocation[0],
                			((((akSurfaces[0].VBuffer.GetPosition3fY(j) * 2.0f * maxBox) + yBox) / 2.0f) * direction[1]) +
                			startLocation[1],
                			((((akSurfaces[0].VBuffer.GetPosition3fZ(j) * 2.0f * maxBox) + zBox) / 2.0f) * direction[2]) +
                			startLocation[2] );

                }

                //double[][] inverseDicomArray = null;
                TransMatrix inverse_DicomMatrix = null;
                if (m_kVolumeViewer.getImageA().getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                	inverse_DicomMatrix = m_kVolumeViewer.getImageA().getMatrix().clone();
                	inverse_DicomMatrix.Inverse();
                	//inverseDicomArray = inverseDicomMatrix.getMatrix();
                }

                TriMesh surfaceSave = new TriMesh( kVBuffer, akSurfaces[0].IBuffer );
                //int iType = akSurfaces[0] instanceof ClodMesh ? 1 : 0;
                FileSurface_WM.saveProstateSurface(kName, surfaceSave, 0, surfaceSave.VBuffer, true, direction, startLocation, box, inverse_DicomMatrix, m_kVolumeViewer.getActiveImage());
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to save single mesh");
            }
        }
    }
    
    
    /**
     * Turn backface culling on/off for the selected surfaces.
     * @param aiSelected selected surfaces.
     */
    private void setBackface(int[] aiSelected) {
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_akSurfaceStates.get( aiSelected[i] ).BackfaceCull = surfaceBackFaceCB.isSelected();
                m_kVolumeViewer.setBackface( (String)kList.elementAt(aiSelected[i]),
                        surfaceBackFaceCB.isSelected());
            }

        }
    }
    
    /**
     * Turns Clipping on/off for the selected surfaces.
     * @param  aiSelected  the list of selected surfaces (SurfaceAttributes)
     */
    private void setClipping(int[] aiSelected) {
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_akSurfaceStates.get( aiSelected[i] ).Clip = surfaceClipCB.isSelected();
                m_kVolumeViewer.setClipping( (String)kList.elementAt(aiSelected[i]),
                                             surfaceClipCB.isSelected());
            }

        }
    }
    
    /**
     * Sets the surface options GUI panel to enabled or disabled. If there are 0 or multiple surfaces selected, all the
     * options should be disabled.
     *
     * @param  flag  Enable or disable.
     */
    private void setElementsEnabled(boolean flag) {
        saveSurfaceButton.setEnabled(flag);
        savePLYSurfaceButton.setEnabled(flag);
        saveSTLSurfaceButton.setEnabled(flag);
        
        decimateButton.setEnabled(flag);
        colorButton.setEnabled(flag);
        subdivideTriangles.setEnabled(flag);
        convexHull.setEnabled(flag);
        extractConnected.setEnabled(flag);
        invertNormals.setEnabled(flag);
        smooth1Button.setEnabled(flag);
        smooth2Button.setEnabled(flag);
        smooth3Button.setEnabled(flag);

        colorLabel.setEnabled(flag);
        m_kAdvancedMaterialOptionsButton.setEnabled(flag);
        m_kSurfaceTextureButton.setEnabled(flag);
        opacityLabel.setEnabled(flag);
        opacitySlider.setEnabled(flag);
        triangleLabel.setEnabled(flag);
        triangleText.setEnabled(flag);
        volumeLabel.setEnabled(flag);
        volumeText.setEnabled(flag);
        areaLabel.setEnabled(flag);
        areaText.setEnabled(flag);

        for (int i = 0; i < opacitySliderLabels.length; i++) {
            opacitySliderLabels[i].setEnabled(flag);
        }

        polygonModeCB.setEnabled(flag);
        comboLabel.setEnabled(flag);
        surfacePickableCB.setEnabled(flag);
        surfaceClipCB.setEnabled(flag);
        surfaceBackFaceCB.setEnabled(flag);
        surfaceTransparencyCB.setEnabled(flag);
        m_kSurfacePaint.setEnabled(flag);
    }
    
    /**
     * Turn picking culling on/off for the selected surfaces.
     * @param aiSelected selected surfaces.
     */
    private void setPickable(int[] aiSelected) {
        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_akSurfaceStates.get( aiSelected[i] ).Pickable = surfacePickableCB.isSelected();
                m_kVolumeViewer.setPickable( (String)kList.elementAt(aiSelected[i]),
                        surfacePickableCB.isSelected());
            }
        }
    }
    
    /**
     * Turns Transparency on/off for the selected surfaces.
     * @param  aiSelected  the list of selected surfaces (SurfaceAttributes)
     */
    private void setTransparency(int[] aiSelected) {

        if ( m_kVolumeViewer != null )
        {
            float opacity = opacitySlider.getValue() / 100.0f;
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            for (int i = 0; i < aiSelected.length; i++) {
                m_akSurfaceStates.get( aiSelected[i] ).Opacity = opacity;

                m_akSurfaceStates.get( aiSelected[i] ).TransparencyOn = surfaceTransparencyCB.isSelected();
                if ( surfaceTransparencyCB.isSelected() )
                {
                    m_kVolumeViewer.setTransparency( (String)kList.elementAt(aiSelected[i]), opacity);
                }
            }
        }
    }
    
    /**
     * Smoothes the selected surfaces. One dialog per group of selected surfaces is displayed (not a different dialog
     * per-surface).
     *
     * @param  aiSelected    the list of selected surfaces (SurfaceAttributes)
     * @param  iSmoothType  the level of smoothing JDialogSmoothMesh.SMOOTH1, JDialogSmoothMesh.SMOOTH2, or
     *                     JDialogSmoothMesh.SMOOTH3
     */
    private void smoothSurface(int[] aiSelected, int iSmoothType )
    {

        if (aiSelected == null) {
            MipavUtil.displayError("Select a surface to smooth.");
            return;
        }

        JDialogSmoothMesh dialog = new JDialogSmoothMesh(null, true, iSmoothType);

        if (dialog.isCancelled()) {
            return;
        }

        if ( m_kVolumeViewer != null )
        {
            DefaultListModel kList = (DefaultListModel)surfaceList.getModel();
            
            int numTriangles = 0;
            float volume = 0;
            float area = 0;
            
            for (int i = 0; i < aiSelected.length; i++) {

                TriMesh kMesh = m_akSurfaceStates.get(aiSelected[i]).Surface;
                int iTarget = 0;
                if (kMesh instanceof ClodMesh) {
                    iTarget = ((ClodMesh)kMesh).TargetRecord();
                    ((ClodMesh)kMesh).TargetRecord(0);
                    ((ClodMesh)kMesh).SelectLevelOfDetail();
                }
                
                if (iSmoothType == JDialogSmoothMesh.SMOOTH1) {
                    m_kVolumeViewer.smoothMesh((String)kList.elementAt(aiSelected[i]),
                            dialog.getIterations(),
                            dialog.getAlpha(),
                            dialog.getVolumeLimit(),
                            dialog.getVolumePercent());
                }
                else if (iSmoothType == JDialogSmoothMesh.SMOOTH2) {
                    m_kVolumeViewer.smoothTwo((String)kList.elementAt(aiSelected[i]),
                            dialog.getIterations(), dialog.getStiffness(), dialog.getVolumeLimit(),
                                        dialog.getVolumePercent());
                }

                else {
                    m_kVolumeViewer.smoothThree((String)kList.elementAt(aiSelected[i]),
                            dialog.getIterations(), dialog.getLambda(), dialog.getMu());
                }

/*
                numTriangles += meshes[j].getIndexCount();
                volume += meshes[j].volume();
                area += meshes[j].area();
                */
                
                if ((kMesh instanceof ClodMesh) && (iTarget != 0 )) {
                    ((ClodMesh)kMesh).TargetRecord(iTarget);
                    ((ClodMesh)kMesh).SelectLevelOfDetail();
                }
            }

            triangleText.setText(String.valueOf(numTriangles / 3));

            // One length across the extracted surface changes from -1 to 1
            // while one length across the actual volume changes by ((dim-1)*res)max
            volumeText.setText("" + volume);
            areaText.setText("" + area);
        }
    }
    
    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** Color Button */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  DOCUMENT ME!
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
            m_kVolumeViewer.updatePlanes();
        }
    }
    
    
}
