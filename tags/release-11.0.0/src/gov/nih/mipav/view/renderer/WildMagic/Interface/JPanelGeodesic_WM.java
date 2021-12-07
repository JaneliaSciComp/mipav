package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.Geodesic_WM;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

import WildMagic.LibGraphics.Collision.PickRecord;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Geometry;
import WildMagic.LibGraphics.SceneGraph.TriMesh;


/**
 * <p>Title: JPanelGeodesic</p>
 *
 * <p>Description: Geodesic drawing interface. Drawing the Geodesic curve on the surfaces. </p>
 *
 * <p>This file also includes the interface for cutting the mesh along the geodesic curve. The mesh can be cut along an
 * open curve (which replaces the original mesh in the scene graph) or the mesh can be cut along a closed curve (which
 * replaces the original mesh with two or more new meshes in the scene graph.</p>
 *
 * <p>The user may display the geodesic in one of three ways: (1) the smoothed geodesic curve, which is based on a
 * re-triangulation of the surface along Dijkstra's path and a smoothed version of Dijkstra's path. (2) Dijkstra's path
 * along the original mesh vertices and triangle edges. (3) The straight-line distance between points -- Euclidian
 * distance. Of the three display modes the Euclidian path is the only path *NOT* constrained to lie on the surface of
 * the triangle mesh.</p>
 *
 * <p>Distances for all three path types, for the total path lengths and the most recent point-pairs are displayed in the
 * interface as well.</p>
 *
 * @author  Alexandra Bokinsky, Ph.D.
 */
public class JPanelGeodesic_WM  extends JInterfaceBase
{

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4382184418236793404L;

    /** Geodesic line draw control label. */
    private JLabel drawLabel;

    /** Current Dijkstra path length. */
    private float m_fDijkstraCurrent = 0;

    /** Previous Dijkstra path length. */
    private float m_fDijkstraPrevious = 0;

    /** Total Dijkstra path length. */
    private float m_fDijkstraTotal = 0;

    /** Values for the current path length between the last two selected points: */
    private float m_fEuclidianCurrent = 0;

    /** Values for the previous current path length between the last two selected points: */
    private float m_fEuclidianPrevious = 0;

    /** Values for the total path lengths:. */
    private float m_fEuclidianTotal = 0;

    /** Current Geodesic path length */
    private float m_fGeodesicSmoothCurrent = 0;

    /** Previous Geodesic path length */
    private float m_fGeodesicSmoothPrevious = 0;

    /** Total Geodesic path length */
    private float m_fGeodesicSmoothTotal = 0;

    /** Toggle between LiveWire Interaction and point & click interaction:. */
    private JCheckBox m_kCheckLivewire;

    /** Clearsall cuts. */
    private JButton m_kClearAllCutsButton;

    /** Clearsall Geodesic components. */
    private JButton m_kClearAllGeodesicButton;

    /** Clear the last cut. */
    private JButton m_kClearLastCutButton;

    /** Button for deleting the geodesic. */
    private JButton m_kClearLastGeodesicButton;

    /** Button for cutting the mesh along the geodesic:. */
    private JButton m_kCutGeodesicButton;

    /** Label displaying the current Dijkstra path length. */
    private JLabel m_kDijkstraDistance;

    /** Label displaying the previous Dijkstra path length. */
    private JLabel m_kDijkstraDistanceValueLast;

    /** Label displaying the total Dijkstra path length.*/
    private JLabel m_kDijkstraDistanceValueTotal;

    /** ButtonGroup for the different path display options. */
    private ButtonGroup m_kDisplayButtonGroup;

    /** Display Dijkstra's path. */
    private JRadioButton m_kDisplayDijkstra;

    /** Display the Euclidian path. */
    private JRadioButton m_kDisplayEuclidian;

    /** Radio buttons for displaying the smoothed geodesic, dijkstra's path, or the euclidian path:. */
    private JRadioButton m_kDisplayGeodesic;

    /** Geodesic drawing interface Toggle button is down while points are added to the curve:. */
    private JToggleButton m_kDrawGeodesicButton;

    /**
     *  Label displaying the current Euclidian path length.
     */
    private JLabel m_kEuclidianDistance;

    /**  Label displaying the previous Euclidian path length. */
    private JLabel m_kEuclidianDistanceValueLast;

    /**  Label displaying the total Euclidian path length. */
    private JLabel m_kEuclidianDistanceValueTotal;

    /** Close and finish the Geodesic path. */
    private JButton m_kFinishClosedGeodesicButton;

    /** Finish the Geodesic path without closing. */
    private JButton m_kFinishOpenGeodesicButton;

    /** Label displaying the current Geodesic path length. */
    private JLabel m_kGeodesicSmoothDistance;

    /** Label displaying the previous Geodesic path length. */
    private JLabel m_kGeodesicSmoothDistanceValueLast;

    /** Label displaying the total Geodesic path length. */
    private JLabel m_kGeodesicSmoothDistanceValueTotal;

    /** Toggle wireframe */
    private JToggleButton m_kGeodesicToggleWireframe;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Geodesic reference. */
    private Geodesic_WM surfaceGeodesic;

    /** Label to indicate that enable Surface pickable before drawing geodesic line. */
    private JLabel surPickLabel;

    /**  Surface panel. */
    private JPanelSurface_WM m_kSurfacePanel = null;

    /**
     * Constructor to initialize the geodesic control panel and create geodesic image scene graph.
     * @param  kVolumeViewer  parent frame
     */
    public JPanelGeodesic_WM(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);
        init();
        surfaceGeodesic = new Geodesic_WM();
        surfaceGeodesic.setPanel(this);
    }
    
    /**
     * Command processor to handle the geodesic button events.
     *
     * @param  e  ActionEvent
     */
    public void actionPerformed(ActionEvent e) {
        Object source = e.getSource();
        String command = e.getActionCommand();

        if (command.equals("DrawGeodesic")) {
            drawGeodesic();
        } else if (command.equals("ClearLastGeodesic")) {
            clearGeodesic(false);
        } else if (command.equals("ClearAllGeodesic")) {
            clearGeodesic(true);
        } else if (command.equals("FinishOpenGeodesic")) {
            finishGeodesic(true);
        } else if (command.equals("FinishClosedGeodesic")) {
            finishGeodesic(false);
        } else if (command.equals("ToggleWireframe")) {
            toggleWireframe();
        } else if (command.equals("ToggleLivewire")) {

            if (m_kCheckLivewire.isSelected()) {
                m_kDisplayGeodesic.setSelected(false);
                m_kDisplayGeodesic.setEnabled(false);
                m_kDisplayDijkstra.setSelected(true);
                m_kDisplayEuclidian.setSelected(false);
            } else {
                m_kDisplayGeodesic.setEnabled(true);
            }

            toggleLivewire();
            toggleGeodesicPathDisplay(1);
        } else if (source == m_kDisplayDijkstra) {
            toggleGeodesicPathDisplay(1);
        } else if (source == m_kDisplayEuclidian) {
            toggleGeodesicPathDisplay(2);
        } else if (source == m_kDisplayGeodesic) {
            toggleGeodesicPathDisplay(0);
        } else if (command.equals("CutGeodesic")) {
            cutGeodesic();
        } else if (command.equals("ClearLastCut")) {
            clearLastCut();
        } else if (command.equals("ClearAllCuts")) {
            clearAllCuts();
        }

    }

    /**
     * Add a new geodesic component to the TriMesh surface display.
     * @param kSurface TriMesh surface.
     * @param kNew geodesic component (point, line)
     * @param iGroup display group (Dijkstra, Euclidian, Geodesic).
     */
    public void addGeodesic( TriMesh kSurface, Geometry kNew, int iGroup )
    {
        m_kVolumeViewer.addGeodesic(kSurface, kNew, iGroup);
    }

    /**
     * Add new mesh to the volume rendering.
     * @param  kNew   TriMesh new surface mesh
     */
    public void addSurface(TriMesh kNew) {
        TriMesh[] akSurfaces = new TriMesh[1];
        akSurfaces[0] = kNew;
        m_kSurfacePanel.addSurfaces(akSurfaces);
    }

    /**
     * clearGeodesic: called when the "Clear Geodesic" Button is pressed.
     *
     * @param  bAll  when true deletes all geodesic curves drawn on the surfaces, when false, deletes the last point
     *               drawn
     */
    public void clearGeodesic(boolean bAll) {

        /* Clear all geodesic curves: */
        if (bAll == true) {
            /* disable cuts */
            m_kCutGeodesicButton.setEnabled(false);
            
            /* disable clear all */
            m_kClearAllGeodesicButton.setEnabled(false);
            m_kClearLastGeodesicButton.setEnabled(false);

            /* the m_kDrawGeodesicButton is a toggle button, once clear is
             * pressed, un-toggle the draw button. */
            m_kDrawGeodesicButton.setSelected(false);

            /* Reset the total path lenghts to zero: */
            m_fEuclidianTotal = 0;
            m_fDijkstraTotal = 0;
            m_fGeodesicSmoothTotal = 0;

            /* Reset the current path lenghts to zero: */
            m_fEuclidianCurrent = 0;
            m_fGeodesicSmoothCurrent = 0;
            m_fDijkstraCurrent = 0;

            /* Reset the previous path lenghts to zero: */
            m_fEuclidianPrevious = 0;
            m_fGeodesicSmoothPrevious = 0;
            m_fDijkstraPrevious = 0;
            
        }
        /* Clear last, reenable draw: */
        else {
            m_kDrawGeodesicButton.setSelected(true);
            m_kFinishOpenGeodesicButton.setEnabled(true);
            m_kFinishClosedGeodesicButton.setEnabled(true);

            /* Clearing last works once, so disable clear last, it is re-enabled
             * when a new point is added to the curve: */
            m_kClearLastGeodesicButton.setEnabled(false);

            /* Update the total path lenghts: */
            m_fEuclidianTotal -= m_fEuclidianCurrent;
            m_fGeodesicSmoothTotal -= m_fGeodesicSmoothCurrent;
            m_fDijkstraTotal -= m_fDijkstraCurrent;

            /* Reset the current path lenghts to the previous value: */
            m_fEuclidianCurrent = m_fEuclidianPrevious;
            m_fGeodesicSmoothCurrent = m_fGeodesicSmoothPrevious;
            m_fDijkstraCurrent = m_fDijkstraPrevious;
        }

        /* Update the GUI labels with the new path lenghts: */
        m_kEuclidianDistanceValueLast.setText("Last two points picked: " + m_fEuclidianCurrent);
        m_kEuclidianDistanceValueTotal.setText("Curve Total: " + m_fEuclidianTotal);
        m_kGeodesicSmoothDistanceValueLast.setText("Last two points picked: " + m_fGeodesicSmoothCurrent);
        m_kGeodesicSmoothDistanceValueTotal.setText("Curve Total: " + m_fGeodesicSmoothTotal);
        m_kDijkstraDistanceValueLast.setText("Last two points picked: " + m_fDijkstraCurrent);
        m_kDijkstraDistanceValueTotal.setText("Curve Total: " + m_fDijkstraTotal);

        if (surfaceGeodesic != null) {
            surfaceGeodesic.clear(bAll);
        }

        m_kVolumeViewer.enableGeodesic( m_kDrawGeodesicButton.isSelected() );
    }


    /**
     * Sets all variables to null, disposes, and garbage collects.
     *
     * @param  flag  dispose super or not, not used now.
     */
    public void disposeLocal() {
        if (surfaceGeodesic != null) {
            surfaceGeodesic.dispose();
            surfaceGeodesic = null;
        }
    }

    /**
     * Enables picking points and drawing the Geodesic curve on the surfaces. Depending on the type of surface
     * displayed: either the SurfaceRender or the FlythruRenderer.
     *
     * <p>For the Geodesic object to perform picking and drawing the pickCanvas, GeodesicGroup, and triangle mesh
     * (Surface) objects must be defined, either through the constructor, or as shown here, by the individual access
     * functions.</p>
     */
    public void drawGeodesic() {

        if ((surfaceGeodesic != null)) {
            m_kVolumeViewer.enableGeodesic( m_kDrawGeodesicButton.isSelected() );
            
            surfaceGeodesic.setRadius(0.002f);
            surfaceGeodesic.setEnable(!surfaceGeodesic.getEnable());

            m_kClearAllGeodesicButton.setEnabled(true);
            m_kClearLastGeodesicButton.setEnabled(surfaceGeodesic.getEnable());
            m_kFinishOpenGeodesicButton.setEnabled(surfaceGeodesic.getEnable());
            m_kFinishClosedGeodesicButton.setEnabled(surfaceGeodesic.getEnable());
        }

        m_kDisplayGeodesic.setEnabled(true);
        m_kDisplayDijkstra.setEnabled(true);
        m_kDisplayEuclidian.setEnabled(true);

        if (m_kCheckLivewire.isSelected()) {
            m_kDisplayGeodesic.setEnabled(false);
        }

        m_fEuclidianTotal = 0;
        m_fDijkstraTotal = 0;
        m_fGeodesicSmoothTotal = 0;

    }

    /**
     * When a new line segment is added to the geodesic curve, the Geodesic object enables removing the last point
     * added.
     * @param  bEnable  enable the remove button or not.
     */
    public void enableClearLast(boolean bEnable) {
        m_kClearLastGeodesicButton.setEnabled(bEnable);
    }

    /**
     * When a new line segment is added to the geodesic curve, the Geodesic object enables removing the last point
     * added.
     * @param  bEnable  enable the last cut button or not.
     */
    public void enableClearLastCut(boolean bEnable) {
        m_kClearLastCutButton.setEnabled(bEnable);
    }


    /**
     * The Geodesic object enables cutting the mesh when the line segments are finished, either finished open or
     * finished closed.
     * @param  bEnable  enable the last button or not.
     */
    public void enableCut(boolean bEnable) {
        m_kCutGeodesicButton.setEnabled(bEnable);
    }

    /**
     * finishGeodesic, called when the "Finish Closed" or "Finish Open" buttons are pressed, finish the current geodesic
     * polyline.
     * @param  bOpen  bOpen when true leaves the curve open, if false, then closes the curve by connecting the last and
     *                first points.
     */
    public void finishGeodesic(boolean bOpen) {

        /* enable clear all: */
        m_kClearAllGeodesicButton.setEnabled(true);

        /* disable clear last, and finish buttons: */
        m_kClearLastGeodesicButton.setEnabled(false);
        m_kFinishOpenGeodesicButton.setEnabled(false);
        m_kFinishClosedGeodesicButton.setEnabled(false);

        /* the m_kDrawGeodesicButton is a toggle button, once finish is
         * pressed, un-toggle the draw button: */
        m_kDrawGeodesicButton.setSelected(false);

        /* call Geodesic.finish( bOpen ): and disable drawing */
        if (surfaceGeodesic != null) {
            surfaceGeodesic.finish(bOpen);
            surfaceGeodesic.setEnable(false);
        }
    }

    /**
     * Initialize the mainPanel with the geodesic drawing buttons.
     */
    public void init() {

        JToolBar viewToolBar = new JToolBar();
        viewToolBar.setBorderPainted(true);
        viewToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        viewToolBar.setLayout(new GridBagLayout());
        viewToolBar.setFloatable(false);

        m_kDrawGeodesicButton = new JToggleButton(MipavUtil.getIcon("drawgeodesic.gif"));
        m_kDrawGeodesicButton.setMargin(new Insets(0, 0, 0, 0));
        m_kDrawGeodesicButton.addActionListener(this);
        m_kDrawGeodesicButton.setToolTipText("Start Geodesic");
        m_kDrawGeodesicButton.setActionCommand("DrawGeodesic");
        m_kDrawGeodesicButton.setBorderPainted(false);
        m_kDrawGeodesicButton.setRolloverEnabled(true);
        m_kDrawGeodesicButton.setRolloverIcon(MipavUtil.getIcon("drawgeodesicroll.gif"));
        m_kDrawGeodesicButton.setFocusPainted(false);
        m_kDrawGeodesicButton.setBorder(BorderFactory.createLoweredBevelBorder());
        viewToolBar.add(m_kDrawGeodesicButton);

        m_kFinishOpenGeodesicButton = new JButton(MipavUtil.getIcon("finishopen.gif"));
        m_kFinishOpenGeodesicButton.addActionListener(this);
        m_kFinishOpenGeodesicButton.setActionCommand("FinishOpenGeodesic");
        m_kFinishOpenGeodesicButton.setFont(MipavUtil.font12B);
        m_kFinishOpenGeodesicButton.setToolTipText("Finish Open");
        m_kFinishOpenGeodesicButton.setBorderPainted(false);
        m_kFinishOpenGeodesicButton.setRolloverEnabled(true);
        m_kFinishOpenGeodesicButton.setRolloverIcon(MipavUtil.getIcon("finishopenroll.gif"));
        m_kFinishOpenGeodesicButton.setFocusPainted(false);
        m_kFinishOpenGeodesicButton.setEnabled(false);
        viewToolBar.add(m_kFinishOpenGeodesicButton);

        m_kFinishClosedGeodesicButton = new JButton(MipavUtil.getIcon("finishclose.gif"));
        m_kFinishClosedGeodesicButton.addActionListener(this);
        m_kFinishClosedGeodesicButton.setActionCommand("FinishClosedGeodesic");
        m_kFinishClosedGeodesicButton.setFont(MipavUtil.font12B);
        m_kFinishClosedGeodesicButton.setToolTipText("Finish Closed");
        m_kFinishClosedGeodesicButton.setBorderPainted(false);
        m_kFinishClosedGeodesicButton.setRolloverEnabled(true);
        m_kFinishClosedGeodesicButton.setRolloverIcon(MipavUtil.getIcon("finishcloseroll.gif"));
        m_kFinishClosedGeodesicButton.setFocusPainted(false);
        m_kFinishClosedGeodesicButton.setEnabled(false);
        viewToolBar.add(m_kFinishClosedGeodesicButton);

        m_kClearLastGeodesicButton = new JButton(MipavUtil.getIcon("clearpoint.gif"));
        m_kClearLastGeodesicButton.addActionListener(this);
        m_kClearLastGeodesicButton.setActionCommand("ClearLastGeodesic");
        m_kClearLastGeodesicButton.setFont(MipavUtil.font12B);
        m_kClearLastGeodesicButton.setToolTipText("Clear Last Point");
        m_kClearLastGeodesicButton.setBorderPainted(false);
        m_kClearLastGeodesicButton.setRolloverEnabled(true);
        m_kClearLastGeodesicButton.setRolloverIcon(MipavUtil.getIcon("clearpointroll.gif"));
        m_kClearLastGeodesicButton.setFocusPainted(false);
        m_kClearLastGeodesicButton.setEnabled(false);
        viewToolBar.add(m_kClearLastGeodesicButton);

        m_kClearAllGeodesicButton = new JButton(MipavUtil.getIcon("clearcurves.gif"));
        m_kClearAllGeodesicButton.addActionListener(this);
        m_kClearAllGeodesicButton.setActionCommand("ClearAllGeodesic");
        m_kClearAllGeodesicButton.setFont(MipavUtil.font12B);
        m_kClearAllGeodesicButton.setToolTipText("Clear All Curves");
        m_kClearAllGeodesicButton.setBorderPainted(false);
        m_kClearAllGeodesicButton.setRolloverEnabled(true);
        m_kClearAllGeodesicButton.setRolloverIcon(MipavUtil.getIcon("clearcurvesroll.gif"));
        m_kClearAllGeodesicButton.setFocusPainted(false);
        m_kClearAllGeodesicButton.setEnabled(false);
        viewToolBar.add(m_kClearAllGeodesicButton);

        m_kGeodesicToggleWireframe = new JToggleButton(MipavUtil.getIcon("wireframe.gif"));
        m_kGeodesicToggleWireframe.setMargin(new Insets(0, 0, 0, 0));
        m_kGeodesicToggleWireframe.addActionListener(this);
        m_kGeodesicToggleWireframe.setToolTipText("Toggle Wireframe");
        m_kGeodesicToggleWireframe.setActionCommand("ToggleWireframe");
        m_kGeodesicToggleWireframe.setBorderPainted(false);
        m_kGeodesicToggleWireframe.setRolloverEnabled(true);
        m_kGeodesicToggleWireframe.setRolloverIcon(MipavUtil.getIcon("wireframeroll.gif"));
        m_kGeodesicToggleWireframe.setFocusPainted(false);
        m_kGeodesicToggleWireframe.setBorder(BorderFactory.createLoweredBevelBorder());
        viewToolBar.add(m_kGeodesicToggleWireframe);

        /* CheckBox to toggle between live wire and point and click modes: */
        m_kCheckLivewire = new JCheckBox("Livewire Mode");
        m_kCheckLivewire.addActionListener(this);
        m_kCheckLivewire.setToolTipText("Toggle beteen livewire mode and point and click mode");
        m_kCheckLivewire.setActionCommand("ToggleLivewire");
        m_kCheckLivewire.setEnabled(false);
        m_kCheckLivewire.setSelected(false);
        viewToolBar.add(m_kCheckLivewire);

        /* Display Toolbar and radio buttons for switching between displaying
         * Dijkstra'a path, the smoothed geodesic curve on the surface, or the
         * straight-line (in space) between points:  */
        JToolBar kDisplayToolBar = new JToolBar();
        kDisplayToolBar.setBorderPainted(true);
        kDisplayToolBar.setLayout(new GridBagLayout());
        kDisplayToolBar.setFloatable(false);

        m_kDisplayButtonGroup = new ButtonGroup();
        m_kDisplayGeodesic = new JRadioButton("Display Geodesic Path");
        m_kDisplayGeodesic.addActionListener(this);
        m_kDisplayGeodesic.setToolTipText("Toggle Geodesic");
        m_kDisplayGeodesic.setActionCommand("ToggleGeodesic");
        m_kDisplayGeodesic.setSelected(true);
        m_kDisplayGeodesic.setEnabled(false);
        m_kDisplayButtonGroup.add(m_kDisplayGeodesic);
        kDisplayToolBar.add(m_kDisplayGeodesic);

        m_kDisplayDijkstra = new JRadioButton("Display Dijkstra's Path");
        m_kDisplayDijkstra.addActionListener(this);
        m_kDisplayDijkstra.setToolTipText("Toggle Dijkstra");
        m_kDisplayDijkstra.setActionCommand("ToggleDijkstra");
        m_kDisplayDijkstra.setSelected(false);
        m_kDisplayDijkstra.setEnabled(false);
        m_kDisplayButtonGroup.add(m_kDisplayDijkstra);
        kDisplayToolBar.add(m_kDisplayDijkstra);

        m_kDisplayEuclidian = new JRadioButton("Display Euclidian Path");
        m_kDisplayEuclidian.addActionListener(this);
        m_kDisplayEuclidian.setToolTipText("Toggle Euclidian");
        m_kDisplayEuclidian.setActionCommand("ToggleEuclidian");
        m_kDisplayEuclidian.setSelected(false);
        m_kDisplayEuclidian.setEnabled(false);
        m_kDisplayButtonGroup.add(m_kDisplayEuclidian);
        kDisplayToolBar.add(m_kDisplayEuclidian);


        /* Buttons for cutting the mesh along the geodesic curve, and for
         * undoing the last cut, and undoing all cuts: */
        JToolBar kCutToolBar = new JToolBar();
        kCutToolBar.setBorderPainted(true);
        kCutToolBar.setLayout(new GridBagLayout());
        kCutToolBar.setFloatable(false);

        m_kCutGeodesicButton = new JButton("Cut Geodesic");
        m_kCutGeodesicButton.addActionListener(this);
        m_kCutGeodesicButton.setToolTipText("Cut Geodesic");
        m_kCutGeodesicButton.setActionCommand("CutGeodesic");
        m_kCutGeodesicButton.setEnabled(false);
        kCutToolBar.add(m_kCutGeodesicButton);

        m_kClearLastCutButton = new JButton("Clear Last Cut");
        m_kClearLastCutButton.addActionListener(this);
        m_kClearLastCutButton.setToolTipText("Clear Last Cut");
        m_kClearLastCutButton.setActionCommand("ClearLastCut");
        m_kClearLastCutButton.setEnabled(false);
        kCutToolBar.add(m_kClearLastCutButton);

        m_kClearAllCutsButton = new JButton("Clear All Cuts");
        m_kClearAllCutsButton.addActionListener(this);
        m_kClearAllCutsButton.setToolTipText("Clear All Cuts");
        m_kClearAllCutsButton.setActionCommand("ClearAllCuts");
        m_kClearAllCutsButton.setEnabled(false);
        kCutToolBar.add(m_kClearAllCutsButton);


        JPanel panel2 = new JPanel(new GridBagLayout());
        GridBagConstraints kGBC = new GridBagConstraints();
        kGBC.anchor = GridBagConstraints.WEST;
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        panel2.add(viewToolBar, kGBC);
        kGBC.gridx = 0;
        kGBC.gridy = 1;
        panel2.add(kDisplayToolBar, kGBC);
        kGBC.gridx = 0;
        kGBC.gridy = 2;
        panel2.add(kCutToolBar, kGBC);

        JPanel panelLabel = new JPanel(new BorderLayout());
        drawLabel = new JLabel("Ctrl and left mouse press to draw the geodesic line.");
        surPickLabel = new JLabel("Enable surface pickable before draw the geodesic line.");
        panelLabel.add(drawLabel, BorderLayout.NORTH);
        panelLabel.add(surPickLabel, BorderLayout.CENTER);

        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.add(panel2);
        contentBox.add(panelLabel);


        m_kEuclidianDistance = new JLabel("Euclidian Distance:");
        m_kEuclidianDistanceValueLast = new JLabel("Last two points picked: " + 0);
        m_kEuclidianDistanceValueTotal = new JLabel("Curve total: " + 0);
        m_kGeodesicSmoothDistance = new JLabel("Geodesic Smoothed:");
        m_kGeodesicSmoothDistanceValueLast = new JLabel("Last two points picked: " + 0);
        m_kGeodesicSmoothDistanceValueTotal = new JLabel("Curve total: " + 0);
        m_kDijkstraDistance = new JLabel("Dijkstra's Path:");
        m_kDijkstraDistanceValueLast = new JLabel("Last two points picked: " + 0);
        m_kDijkstraDistanceValueTotal = new JLabel("Curve total: " + 0);

        JPanel kDistanceLabelsPanel = new JPanel(new GridBagLayout());
        kGBC.anchor = GridBagConstraints.WEST;
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        kDistanceLabelsPanel.add(m_kEuclidianDistance, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 1;
        kDistanceLabelsPanel.add(m_kEuclidianDistanceValueLast, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 2;
        kDistanceLabelsPanel.add(m_kEuclidianDistanceValueTotal, kGBC);
        kGBC.gridx = 0;
        kGBC.gridy = 3;
        kDistanceLabelsPanel.add(m_kGeodesicSmoothDistance, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 4;
        kDistanceLabelsPanel.add(m_kGeodesicSmoothDistanceValueLast, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 5;
        kDistanceLabelsPanel.add(m_kGeodesicSmoothDistanceValueTotal, kGBC);
        kGBC.gridx = 0;
        kGBC.gridy = 6;
        kDistanceLabelsPanel.add(m_kDijkstraDistance, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 7;
        kDistanceLabelsPanel.add(m_kDijkstraDistanceValueLast, kGBC);
        kGBC.gridx = 1;
        kGBC.gridy = 8;
        kDistanceLabelsPanel.add(m_kDijkstraDistanceValueTotal, kGBC);

        contentBox.add(kDistanceLabelsPanel);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.add(contentBox, BorderLayout.NORTH);


        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(scroller, BorderLayout.NORTH);
        setEnabled(false);
    }

    
    /**
     * Check whether the Geodesic drawing is enabled or not.
     * @return  boolean <code>true</code> Geodesic drawing enabled, <code>false</code> Geodesic disable.
     */
    public boolean isGeodesicEnable() {
        return surfaceGeodesic.getEnable();
    }
    
    
    /**
     * Removes all geodesic curves for the given surface.
     * @param kSurface the surface to modify.
     */
    public void removeAllGeodesic( TriMesh kSurface )
    {
        m_kVolumeViewer.removeAllGeodesic(kSurface);
    }    
    
    /**
     * Remove the specific geodesic curves from the given surface.
     * @param kSurface the surface to modify.
     * @param iNode the node to remove.
     * @param iGroup the group the node belongs to.
     */
    public void removeGeodesic( TriMesh kSurface, int iNode, int iGroup )
    {
        m_kVolumeViewer.removeGeodesic(kSurface, iNode, iGroup);
    }
    
    /**
     * When the Geodesic object cuts the mesh along an open curve, the old mesh changes, but does not need to be deleted
     * and no new mesh needs to be added. This function allows the Geodesic object to replace the original mesh with the
     * sliced mesh in the surface renderer. ReplaceMesh is also used to undo cutting operations.
     *
     * @param  kOld  TriMesh old surface mesh
     * @param  kNew  TriMesh new surface mesh
     */
    public void replaceMesh(TriMesh kOld, TriMesh kNew) {

        m_kVolumeViewer.replaceGeodesic(kOld, kNew);
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /**
     * Displays the Geodesic (dijkstra's along the mesh) distance between the last two points picked in the Geodesic
     * class, as well as the running total for the current curve.
     *
     * @param  fValue  dijkstra's current value.
     */
    public void setDijkstra(float fValue) {

        /* Store the previous value, in case the last point is removed: */
        m_fDijkstraPrevious = m_fDijkstraCurrent;

        /* Update the current value and GUI: */
        m_fDijkstraCurrent = fValue;
        m_kDijkstraDistanceValueLast.setText("Last two points picked: " + m_fDijkstraCurrent);

        /* Update the total value and GUI: */
        m_fDijkstraTotal += fValue;
        m_kDijkstraDistanceValueTotal.setText("Curve Total: " + m_fDijkstraTotal);
    }

    /**
     * Since geodesic line only apply to the texture render surface, all the buttons are enabled only if the suface
     * vector size is greater than 0. The method is called from the surfacePanel when add or remove surfaces.
     *
     * @param  flag  boolean
     */
    public void setEnabled(boolean flag) {
        m_kDrawGeodesicButton.setEnabled(flag);
        m_kFinishOpenGeodesicButton.setEnabled(false);
        m_kFinishClosedGeodesicButton.setEnabled(false);
        m_kGeodesicToggleWireframe.setEnabled(flag);
        m_kCheckLivewire.setEnabled(false);
        m_kClearLastGeodesicButton.setEnabled(false);
        m_kClearAllGeodesicButton.setEnabled(false);
        drawLabel.setEnabled(flag);
        surPickLabel.setEnabled(flag);
    }


    /**
     * Displays the Euclidian distance between the last two points picked in the Geodesic class, as well as the running
     * total for the current curve.
     *
     * @param  fValue  current Eclidian value.
     */
    public void setEuclidian(float fValue) {

        /* Store the previous value, in case the last point is removed: */
        m_fEuclidianPrevious = m_fEuclidianCurrent;

        /* Update the current value and GUI: */
        m_fEuclidianCurrent = fValue;
        m_kEuclidianDistanceValueLast.setText("Last two points picked: " + m_fEuclidianCurrent);

        /* Update the total value and GUI: */
        m_fEuclidianTotal += fValue;
        m_kEuclidianDistanceValueTotal.setText("Curve Total: " + m_fEuclidianTotal);
    }

    /**
     * Displays the Geodesic (smoothed) distance between the last two points picked in the Geodesic class, as well as
     * the running total for the current curve.
     *
     * @param  fValue  the current geodesic smooth value.
     */
    public void setGeodesicSmooth(float fValue) {

        /* Store the previous value, in case the last point is removed: */
        m_fGeodesicSmoothPrevious = m_fGeodesicSmoothCurrent;

        /* Update the current value and GUI: */
        m_fGeodesicSmoothCurrent = fValue;
        m_kGeodesicSmoothDistanceValueLast.setText("Last two points picked: " + m_fGeodesicSmoothCurrent);

        /* Update the total value and GUI: */
        m_fGeodesicSmoothTotal += fValue;
        m_kGeodesicSmoothDistanceValueTotal.setText("Curve Total: " + m_fGeodesicSmoothTotal);
    }

    /**
     * Set the picked point.
     * @param kPickPoint PickRecord.
     * @param kMesh picked TriMesh
     */
    public void setPickedPoint( PickRecord kPickPoint, TriMesh kMesh )
    {
        if ( surfaceGeodesic != null )
        {
            surfaceGeodesic.setPickedPoint( kPickPoint, kMesh );
        }
    }

    /**
     * Set the surface panel.
     * @param kSurfacePanel JPanelSurface
     */
    public void setSurfacePanel( JPanelSurface_WM kSurfacePanel )
    {
        m_kSurfacePanel = kSurfacePanel;
    }


    /**
     * Causes the Geodesic class to switch between displaying the Smoothed Geodesic, Dijkst'ra path along the mesh, or
     * the straight-line Euclidian path between the selected endpoints. Both the Geodesic and Dijkstr'a paths are
     * constrained to lie on the surface of the mesh, whereas the straight-line distance may penetrate the mesh or be
     * through space.
     *
     * @param  which  path index.
     */
    public void toggleGeodesicPathDisplay(int iWhich) {
        if (m_kSurfacePanel != null) {
            m_kSurfacePanel.toggleGeodesicPathDisplay(iWhich);
        }
    }

    /**
     * Toggles between live wire mode and point and click mode for drawing geodesics on the surfaces. When live wire is
     * active, then the use clicks to add the first point in a curve and then moves the mouse to see Dijkstra's path
     * drawn between that point and the Mesh vertex that is nearest the mouse. When the user clicks again the point is
     * drawn, and the displaay is between the last point clicked and the current mouse point.
     */
    public void toggleLivewire() {
        if (surfaceGeodesic != null) {
            surfaceGeodesic.toggleLivewire();
        }
    }
    
    /**
     * Toggles between wireframe and filled polygon drawing for the surfaces. Useful for testing the Geodesic curves:
     */
    public void toggleWireframe() {
        if ( m_kGeodesicToggleWireframe.isSelected() )
        {
            m_kSurfacePanel.changePolyMode(WireframeState.FillMode.FM_LINE);
        }
        else
        {
            m_kSurfacePanel.changePolyMode(WireframeState.FillMode.FM_FILL);
        }
    }

    /* (non-Javadoc)
     * @see java.lang.Object#finalize()
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Undoes all cuts and replaces the cut mesh with the original mesh.
     */
    private void clearAllCuts() {
        if (surfaceGeodesic != null) {
            surfaceGeodesic.clearCut(true);
        }

        m_kCutGeodesicButton.setEnabled(false);
        m_kClearLastCutButton.setEnabled(false);
        m_kClearAllCutsButton.setEnabled(false);
    }

    /**
     * Undoes the last cut operation.
     */
    private void clearLastCut() {
        if (surfaceGeodesic != null) {
            surfaceGeodesic.clearCut(false);
        }

        m_kCutGeodesicButton.setEnabled(false);
        m_kClearLastCutButton.setEnabled(false);
    }

    /**
     * Calls the Geodesic cut function to cut the triangle mesh along the geodesic curves.
     */
    private void cutGeodesic() {
        if (surfaceGeodesic != null) {
            surfaceGeodesic.cut();
        }

        m_kCutGeodesicButton.setEnabled(false);
        m_kClearLastCutButton.setEnabled(true);
        m_kClearAllCutsButton.setEnabled(true);
    }

}
