package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import com.sun.j3d.utils.behaviors.mouse.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.picking.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.Box;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * <p>Title: JPanelClip</p>
 *
 * <p>Description: Clip panel control of the surface renderer. The clipping control includes x, y, z clipping sliders;
 * the arbitrary and static eye clipping sliders.</p>
 *
 * @author  Ruida Cheng
 */
public class JPanelClip extends JPanelRendererJ3D
        implements ChangeListener, MouseListener, MouseBehaviorCallback, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3342110506544352170L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Enable flags for the 6 ( X, Y, Z and inverse ) clipping planes. */
    boolean[] enables = { false, false, false, false, false, false };

    /** Enable flags for the arbitrary clipping planes. */
    boolean[] enablesArbi = { false, false, false, false, false, false };

    /** Enable flags for the static clipping planes. */
    boolean[] enablesStatic = { false, false, false, false, false, false };

    /** Arbitrary clipping plane four corners points. */
    private javax.vecmath.Vector4f[] AclipPlanePts;

    /** Mouse Rotate behavior of the arbitrary clipping plane. */
    private MouseRotate arbiMouseRotateBehavior;

    /** Tranform group for arbitrary clipping plane. */
    private TransformGroup arbiTG;

    /** Transform3D group for clipSliceA frame box and ModelClip arbitrary clipping plane. */
    private Transform3D arbiTrans3d, mcArbiTrans3D;

    /** Arbitrary clipping plane switch group. */
    private Switch arbitrary_SG = new Switch();

    /** Which arbitray clipping slice is currently displayed. */
    private int aSlice;

    /** Value attribute for the roation axis. */
    private float axisX = 0.0f, axisY = 0.0f, axisZ = 0.0f, axisAngle = 0.0f;

    /** The BranchGroup to which the arbitrary behaviors are attached and rotate. */
    private BranchGroup behaviorBG;

    /** Arbitrary and static clipping plane check box. */
    private JCheckBox boundingCheckA, boundingCheckStatic, boundingCheckStaticInv;

    /** Check box to turn the clipping plane frame on and off. */
    private JCheckBox boundingCheckX, boundingCheckY, boundingCheckZ;

    /** X,Y,Z inverse clipping plane check box. */
    private JCheckBox boundingCheckXInv, boundingCheckYInv, boundingCheckZInv;

    /** Static and static inverse, arbitrary clipping plane check box. */
    private JCheckBox boxStatic, boxStaticInv, boxA;

    /** Check boxes that turn the image plane and the sliders on and off. */
    private JCheckBox boxX, boxY, boxZ, boxXInv, boxYInv, boxZInv;

    /** Counters show the event number in the mouse recorder. */
    private int clipCount, rotationCount;

    /** Mouse event vector that record the clipping plane sliders changes. */
    private MouseEventVector clipEvents;

    /** Arbitrary clipping plane yellow boundary indicator box. */
    private ViewJComponentBoxSlice clipSliceAIndicator, clipSliceA;

    /** Static clipping plane boundary box. */
    private ViewJComponentBoxSlice clipSliceStatic, clipSliceStaticInv;

    /** The bounding box frame around the clipping planes. */
    private ViewJComponentBoxSlice clipSliceX, clipSliceY, clipSliceZ;

    /** X, Y, Z inverse clipping slice boundary box. */
    private ViewJComponentBoxSlice clipSliceXInv, clipSliceYInv, clipSliceZInv;

    /** Static, static inverse and arbitrary clipping plane boundary box. */
    private JSlider clipSliderStatic, clipSliderStaticInv, sliderA;

    /** Sliders for the image planes. */
    private JSlider clipSliderX, clipSliderY, clipSliderZ, sliderXInv, sliderYInv, sliderZInv;

    /** Color button for the arbitrary clipping plane frame. */
    private JButton colorButtonA;

    /** Color button for the static clipping plane frame. */
    private JButton colorButtonStatic, colorButtonStaticInv;

    /** Color button for X clipping plane frame. */
    private JButton colorButtonX, colorButtonXInv;

    /** Color button for Y clipping plane frame. */
    private JButton colorButtonY, colorButtonYInv;

    /** Color button for Z clipping plane frame. */
    private JButton colorButtonZ, colorButtonZInv;

    /** Color chooser dialog. */
    private ViewJColorChooser colorChooser;

    /** Vetor holding the clipping plane equations. */
    private Vector4d[] eqnPlanes, eqnPlanesArbi, eqnPlanesStatic;

    /** Vector4D clipping equation of each clipping plane. */
    private Vector4d eqnX, eqnXInv, eqnY, eqnYInv, eqnZ, eqnZInv, eqnA;

    /** Extract arbitrary cliping plane button. */
    private JButton extractButtonA;

    /** Extract static eye cliping plane button. */
    private JButton extractButtonS;

    /** intersectioin points coordinate. */
    private Point3f[] intersectionPts;

    /** 12 possible intersection points. */
    private int iNumLights = 12;

    /** Flag to indicate 6 plane clipping is active. */
    private boolean is6PlaneClipping = true;

    /** Arbitrary clipping plane frame being picked or not. */
    private boolean isClipArbiPicked = false;

    /** First time to build the clipping plane tree. */
    private boolean isFirstTimeBuildTree = false;

    /** Flag to indicate StateChangeEvent invoked. */
    private boolean isStateChangedEvent = false;

    /** Flag to indicate tranformChanged event invoked. */
    private boolean isTransformChanged = false;

    /** Arbitrary clipping slider labels. */
    private JLabel labelAStart, labelAMid, labelAEnd;

    /** Static cliiping slider labels. */
    private JLabel labelStatic, labelStaticInv, labelA;

    /** Static inverse clipping plane labels. */
    private JLabel labelStaticInvStart, labelStaticInvMid, labelStaticInvEnd;

    /** Static clipping plane labels. */
    private JLabel labelStaticStart, labelStaticMid, labelStaticEnd;

    /** Sliders labels. */
    private JLabel labelX, labelY, labelZ, labelXInv, labelYInv, labelZInv;

    /** Slider tick labels. */
    private JLabel labelXStart, labelXMid, labelXEnd;

    /** X clipping slider labels. */
    private JLabel labelXStartInv, labelXMidInv, labelXEndInv;

    /** Y clipping slider labels. */
    private JLabel labelYStart, labelYMid, labelYEnd;

    /** Y inverse clipping slider labels. */
    private JLabel labelYStartInv, labelYMidInv, labelYEndInv;

    /** Z clipping slider labels. */
    private JLabel labelZStart, labelZMid, labelZEnd;

    /** Z clipping slider labels. */
    private JLabel labelZStartInv, labelZMidInv, labelZEndInv;

    /** Backup of the data for undo:. */
    private int[] m_aiImageA_backup;

    /** ImageB backup buffer. */
    private int[] m_aiImageB_backup;

    /** Parent of each light bulb. */
    private BranchGroup[] m_akLightBG;

    /** Material properties for each light bulb. */
    private Material[] m_akLightMaterials;

    /** Shapes representing light bulbs. */
    private Sphere[] m_akLightSpheres;

    /** Parent transform group. */
    private TransformGroup[] m_akLightTransformGroup;

    /** Parent of all the light bulbs. */
    private BranchGroup m_kLightRoot;

    /** The largest of xBox,yBox,and zBox. */
    private float maxBox;

    /** The largest dimension of xDim, yDim and zDim. */
    private int maxDim;

    /** Global model clip object. */
    private ModelClip mc, mcArbi, mcStatic;

    /** Branch group of the arbitrary clipping plane. */
    private BranchGroup mcArbiBG, mcExtendBG, mcArbiExtendBG;

    /** ModelClip arbitrary TransformGroup. */
    private TransformGroup mcArbiTG;

    /** Branch group of the 6 clipping planes. */
    private BranchGroup mcBG;

    /** Branch group of the static clipping plane. */
    private BranchGroup mcStaticBG;

    /** Arbitrary clipping branch group. */
    private BranchGroup objClipSliceA_BG, clipSliceA_BG, clipSliceAIndicator_BG;

    /** root of all the clipping slices branch group. */
    private BranchGroup objClipSlices_BG;

    /** Static clipping branch group. */
    private BranchGroup objClipSliceStatic_BG, objClipSliceStaticInv_BG;

    /** Parent for each individual clip slices branch group, childen of objClipSlices_BG. */
    private BranchGroup objClipSliceX_BG, objClipSliceY_BG, objClipSliceZ_BG;

    /** Parent of the -x, -y, -z clip slices branchgroup. */
    private BranchGroup objClipSliceXInv_BG, objClipSliceYInv_BG, objClipSliceZInv_BG;

    /** Arbitrary and static control panels. */
    private JPanel panelA, panelS, panelSInv;

    /** Declare each clipping plane control panel. */
    private JPanel panelX, panelXInv, panelY, panelYInv, panelZ, panelZInv;

    /** Arbitrary clipping plane expanding ratio. */
    private float radicalRatio = (float) Math.sqrt(2);

    /** Flag indicates whether to record the clip event name. */
    private boolean recordEventName;

    /**
     * Rotation axis to sync up the action for both ModelClip arbitrary clipping plane and the clipSliceA frame box
     * transformGroup.
     */
    private AxisAngle4f rotationAxis;

    /** mouse rotation event vector for the arbitrary clipping plane. */
    private MouseEventVector rotationEvent;

    /** The scroll pane holding the panel content. Used when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding all the control components. */
    private DrawingPanel scrollPanel;

    /** Record the current static clipping slice number. */
    private int sSlice, sSliceInv;

    /** Tabbed Panel that hold the each clipping planes control box. */
    private JTabbedPane tabbedPane;

    /** Static and arbitrary text field. */
    private JTextField textStatic, textStaticInv, textA;

    /** Text fields that display the slice number when slider moves. */
    private JTextField textX, textY, textZ;

    /** X, Y, Z inverse text field. */
    private JTextField textXInv, textYInv, textZInv;

    /** xBox, yBox, zBox from the parent scene. */
    private float xBox, yBox, zBox;

    /** Image X, Y, Z dimension. */
    private int xDim, yDim, zDim;

    /** Which slice is currently displayed in the ZY plane. */
    private int xSlice, xSliceInv;

    /** Which slice is currently displayed in the XZ plane. */
    private int ySlice, ySliceInv;

    /** Which slice is currently displayed in the XY plane. */
    private int zSlice, zSliceInv;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * 3D texture surface renderer clipping dialog control.
     *
     * @param  parent  SurfaceRender Surface renderer reference
     * @param  xBox    float unit box x length
     * @param  yBox    float unit box y length
     * @param  zBox    float unit box z length
     */
    public JPanelClip(SurfaceRender parent, float xBox, float yBox, float zBox) {
        super(parent);
        xDim = renderBase.getImageA().getExtents()[0];
        yDim = renderBase.getImageA().getExtents()[1];
        zDim = renderBase.getImageA().getExtents()[2];
        xSlice = (xDim - 1) / 2;
        ySlice = (yDim - 1) / 2;
        zSlice = (zDim - 1) / 2;

        this.xBox = xBox;
        this.yBox = yBox;
        this.zBox = zBox;
        maxBox = Math.max(xBox, Math.max(yBox, zBox));
        maxDim = Math.max(xDim, Math.max(yDim, zDim));

        eqnX = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnXInv = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnY = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnYInv = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnZ = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnZInv = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnA = new Vector4d(0.0, 0.0, 0.0, 0.0);
        eqnPlanes = new Vector4d[] { eqnX, eqnXInv, eqnY, eqnYInv, eqnZ, eqnZInv };
        eqnPlanesArbi = new Vector4d[] { eqnA, eqnXInv, eqnY, eqnYInv, eqnZ, eqnZInv };
        eqnPlanesStatic = new Vector4d[] { eqnA, eqnXInv, eqnY, eqnYInv, eqnZ, eqnZInv };

        isFirstTimeBuildTree = true;

        // Build dialog.
        init();

        // init intersection points
        initSphereBranch();
        backupImage(((SurfaceRender) renderBase).getImageA(), ((SurfaceRender) renderBase).getImageB());
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Changes color of slices box frame and button if color button was pressed; turns bounding box on and off if
     * checkbox was pressed.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if ((source == extractButtonA) || (source == extractButtonS)) {
            // extract arbitrary clipping plane.
        	WildMagic.LibFoundation.Mathematics.Vector3f[] pts = getAClipPlanePts();
            ModelImage img = renderBase.getImageA();
            
            final float xRes = img.getFileInfo(0).getResolutions()[0];
            final float yRes = img.getFileInfo(0).getResolutions()[1];
            final float zRes = img.getFileInfo(0).getResolutions()[2];

            int length = (int)
                             Math.round(Math.sqrt(((pts[2].X - pts[0].X) * (pts[2].X - pts[0].X)) +
                                                  ((pts[2].Y - pts[0].Y) * (pts[2].Y - pts[0].Y)) +
                                                  ((pts[2].Z - pts[0].Z) * (pts[2].Z - pts[0].Z))));
            int width = (int)
                    Math.round(Math.sqrt(((pts[1].X - pts[0].X) * (pts[1].X - pts[0].X)) +
                                         ((pts[1].Y - pts[0].Y) * (pts[1].Y - pts[0].Y)) +
                                         ((pts[1].Z - pts[0].Z) * (pts[1].Z - pts[0].Z))));
            float newXRes = 
            		Math.round(Math.sqrt(xRes * ((pts[1].X - pts[0].X) * (pts[1].X - pts[0].X)) +
                                         yRes * ((pts[1].Y - pts[0].Y) * (pts[1].Y - pts[0].Y)) +
                                         zRes * ((pts[1].Z - pts[0].Z) * (pts[1].Z - pts[0].Z))));
            float newYRes = 
            		Math.round(Math.sqrt(xRes * ((pts[2].X - pts[0].X) * (pts[2].X - pts[0].X)) +
            				             yRes * ((pts[2].Y - pts[0].Y) * (pts[2].Y - pts[0].Y)) +
                                         zRes * ((pts[2].Z - pts[0].Z) * (pts[2].Z - pts[0].Z))));
            newXRes /= width;
            newYRes /= length;

            int[] ext = new int[]{width,length};
            float[] values = new float[ext[0] * ext[1]];
            try {
            	WildMagic.LibFoundation.Mathematics.Vector3f temp = new WildMagic.LibFoundation.Mathematics.Vector3f( pts[2] );
            	pts[2].Copy( pts[3] );
            	pts[3].Copy( temp );
				img.exportDiagonal(0, 0, ext, pts, values, true);
			} catch (IOException e) { }
            ModelImage resultImage = new ModelImage(img.getType(), ext, "Image plane");
            resultImage.setResolutions( new float[]{newXRes, newYRes} );
            try {
            	resultImage.importData(0, values, true);
            } catch (IOException er) {
                return;
            }
            new ViewJFrameImage(resultImage, null, new Dimension(610, 200));            
            
        } else if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheckX) {

            if (boundingCheckX.isSelected()) {
                addClipSliceX();
                colorButtonX.setEnabled(true);
            } else {
                hideClipSliceX();
                colorButtonX.setEnabled(false);
            }
        } else if (source == boundingCheckXInv) {

            if (boundingCheckXInv.isSelected()) {
                addClipSliceXInv();
                colorButtonXInv.setEnabled(true);
            } else {
                hideClipSliceXInv();
                colorButtonXInv.setEnabled(false);
            }
        } else if (source == boundingCheckY) {

            if (boundingCheckY.isSelected()) {
                addClipSliceY();
                colorButtonY.setEnabled(true);
            } else {
                hideClipSliceY();
                colorButtonY.setEnabled(false);
            }
        } else if (source == boundingCheckYInv) {

            if (boundingCheckYInv.isSelected()) {
                addClipSliceYInv();
                colorButtonYInv.setEnabled(true);
            } else {
                hideClipSliceYInv();
                colorButtonYInv.setEnabled(false);
            }
        } else if (source == boundingCheckZ) {

            if (boundingCheckZ.isSelected()) {
                addClipSliceZ();
                colorButtonZ.setEnabled(true);
            } else {
                hideClipSliceZ();
                colorButtonZ.setEnabled(false);
            }
        } else if (source == boundingCheckZInv) {

            if (boundingCheckZInv.isSelected()) {
                addClipSliceZInv();
                colorButtonZInv.setEnabled(true);
            } else {
                hideClipSliceZInv();
                colorButtonZInv.setEnabled(false);
            }
        } else if (source == boundingCheckA) {

            if (boundingCheckA.isSelected()) {
                addClipSliceA();
                colorButtonA.setEnabled(true);
                enableClipArbiBehavior(false);
            } else {
                hideClipSliceA();
                colorButtonA.setEnabled(false);
            }
       } else if (source == boundingCheckStatic) {

            if (boundingCheckStatic.isSelected()) {
                addClipSliceStatic();
                colorButtonStatic.setEnabled(true);
            } else {
                hideClipSliceStatic();
                colorButtonStatic.setEnabled(false);
            }
       } else if (source == boundingCheckStaticInv) {

            if (boundingCheckStaticInv.isSelected()) {
                addClipSliceStaticInv();
                colorButtonStaticInv.setEnabled(true);
            } else {
                hideClipSliceStaticInv();
                colorButtonStaticInv.setEnabled(false);
            }
        } else if (command.equals("+X")) {

            if (!boxX.isSelected()) {
                setXSliderEnabled(false);

                removeClipSliceX();
                boundingCheckX.setSelected(false);
                initClipSliceX();
                colorButtonX.setEnabled(false);

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }
            } else {
                swapModelClipBG(true);
                setXSliderEnabled(true);
                updateClipSliceX();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("+Y")) {
            if (!boxY.isSelected()) {
                setYSliderEnabled(false);
                colorButtonY.setEnabled(false);
                removeClipSliceY();
                boundingCheckY.setSelected(false);
                initClipSliceY();

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }

            } else {
                swapModelClipBG(true);
                setYSliderEnabled(true);
                updateClipSliceY();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("+Z")) {
            if (!boxZ.isSelected()) {
                setZSliderEnabled(false);
                colorButtonZ.setEnabled(false);
                removeClipSliceZ();
                boundingCheckZ.setSelected(false);
                initClipSliceZ();

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }
            } else {
                swapModelClipBG(true);
                setZSliderEnabled(true);
                updateClipSliceZ();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("-X")) {
            if (!boxXInv.isSelected()) {
                setXSliderInvEnabled(false);
                colorButtonXInv.setEnabled(false);
                removeClipSliceXInv();
                boundingCheckXInv.setSelected(false);
                initClipSliceXInv();

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }
            } else {
                swapModelClipBG(true);
                setXSliderInvEnabled(true);
                updateClipSliceXInv();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("-Y")) {
            if (!boxYInv.isSelected()) {
                setYSliderInvEnabled(false);
                colorButtonYInv.setEnabled(false);
                removeClipSliceYInv();
                boundingCheckYInv.setSelected(false);
                initClipSliceYInv();

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }
            } else {
                swapModelClipBG(true);
                setYSliderInvEnabled(true);
                updateClipSliceYInv();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("-Z")) {
            if (!boxZInv.isSelected()) {
                setZSliderInvEnabled(false);
                colorButtonZInv.setEnabled(false);
                removeClipSliceZInv();
                boundingCheckZInv.setSelected(false);
                initClipSliceZInv();

                if (rayBasedRender != null) {
                    rayBasedRender.updateImages(true);
                }
            } else {
                swapModelClipBG(true);
                setZSliderInvEnabled(true);
                updateClipSliceZInv();
                updateVolumeRenderClipPlane();
                disableClipPlanesArbi();
            }
        } else if (command.equals("A")) {

            if (!boxA.isSelected()) {
                setASliderEnabled(false);
                colorButtonA.setEnabled(false);
                removeClipSliceA();
                hideClipSliceA();
                boundingCheckA.setSelected(false);
                initClipSliceA();
                disableClipPlanesArbi();
            } else {
                swapModelClipBG(false);
                setASliderEnabled(true);
                disableClipPlanes();
                updateClipSliceA();
                colorButtonA.setEnabled(true);
                enableClipArbiBehavior(false);
            }
       } else if (command.equals("EYE")) {

            if (!boxStatic.isSelected()) {
                setStaticSliderEnabled(false);
                colorButtonStatic.setEnabled(false);
                removeClipSliceStatic();
                removeClipSliceStaticInv();
                hideClipSliceStatic();
                boundingCheckStatic.setSelected(false);
                initClipSliceStatic();
                disableStaticClipping();
                disableStaticInvClipping();
                hideClipSliceStaticInv();
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                enableStaticClipping();
                setStaticSliderEnabled(true);
                updateClipSliceStatic();
            }
       } else if (command.equals("-EYE")) {

            if (!boxStaticInv.isSelected()) {
                setStaticInvSliderEnabled(false);
                colorButtonStaticInv.setEnabled(false);
                removeClipSliceStaticInv();
                removeClipSliceStatic();
                hideClipSliceStaticInv();
                hideClipSliceStatic();
                boundingCheckStaticInv.setSelected(false);
                initClipSliceStaticInv();
                disableStaticInvClipping();
                disableStaticClipping();
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                enableStaticInvClipping();
                setStaticInvSliderEnabled(true);
                updateClipSliceStaticInv();
            }
        }

    }

    /**
     * Attach clipping plane branch group.
     */
    public void addClipSlice() {
        addClipSliceX();
        addClipSliceY();
        addClipSliceZ();
        addClipSliceXInv();
        addClipSliceYInv();
        addClipSliceZInv();
    }

    /**
     * Attach arbitrary clip slice bounding frame.
     */
    public void addClipSliceA() {

        if ((objClipSliceA_BG != null) && !objClipSliceA_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceA_BG);
        }

        if (!m_kLightRoot.isLive()) {
            renderBase.getSceneRootTG().addChild(m_kLightRoot);
        }
    }

    /**
     * Attach static clip slice bounding frame.
     */
    public void addClipSliceStatic() {

        if ((objClipSliceStatic_BG != null) && !objClipSliceStatic_BG.isLive()) {

            // objClipSlices_BG.addChild(objClipSliceStatic_BG);
            renderBase.getBranchGroup().addChild(objClipSliceStatic_BG);
        }

        if (!m_kLightRoot.isLive()) {
            renderBase.getSceneRootTG().addChild(m_kLightRoot);
        }

    }

    /**
     * Attach static inverse clip slice bounding frame.
     */
    public void addClipSliceStaticInv() {

        if ((objClipSliceStaticInv_BG != null) && !objClipSliceStaticInv_BG.isLive()) {
            renderBase.getBranchGroup().addChild(objClipSliceStaticInv_BG);
        }

        /*
         * if ( !m_kLightRoot.isLive() ) { renderBase.getSceneRootTG().addChild( m_kLightRoot ); }
         */
    }

    /**
     * Attach X clip slice bounding frame.
     */
    public void addClipSliceX() {

        if ((objClipSliceX_BG != null) && !objClipSliceX_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceX_BG);
        }
    }

    /**
     * Attach X Invative clip slice bounding frame.
     */
    public void addClipSliceXInv() {

        if ((objClipSliceXInv_BG != null) && !objClipSliceXInv_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceXInv_BG);
        }
    }

    /**
     * Attach Y clip slice bounding frame.
     */
    public void addClipSliceY() {

        if ((objClipSliceY_BG != null) && !objClipSliceY_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceY_BG);
        }
    }

    /**
     * Attach Y Invative clip slice bounding frame.
     */
    public void addClipSliceYInv() {

        if ((objClipSliceYInv_BG != null) && !objClipSliceYInv_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceYInv_BG);
        }
    }

    /**
     * Attach Z clip slice bounding frame.
     */
    public void addClipSliceZ() {

        if ((objClipSliceZ_BG != null) && !objClipSliceZ_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceZ_BG);
        }
    }

    /**
     * Attach Z Invative clip slice bounding frame.
     */
    public void addClipSliceZInv() {

        if ((objClipSliceZInv_BG != null) && !objClipSliceZInv_BG.isLive()) {
            objClipSlices_BG.addChild(objClipSliceZInv_BG);
        }
    }

    /**
     * Add a new branch group to model clip for clipping.
     *
     * @param  root  BranchGroup Node.
     */
    public void addToModelClip(BranchGroup root) {
        mc.addScope(root);
        mcArbi.addScope(root);
        mcStatic.addScope(root);
    }

    /**
     * Build cliping planes tree structure. This method being called when clipping plane menu selected in the surface
     * render.
     */
    public void buildClipPlanesTree() {
        Shape3D shape;

        objClipSlices_BG = new BranchGroup();
        objClipSlices_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSlices_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSlices_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSlices_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSlices_BG.setPickable(true);
        renderBase.getSceneRootTG().addChild(objClipSlices_BG);

        objClipSliceA_BG = new BranchGroup();
        objClipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceA_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceA_BG.setPickable(true);

        clipSliceA_BG = new BranchGroup();
        clipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        clipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        clipSliceA_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        clipSliceA_BG.setCapability(BranchGroup.ALLOW_DETACH);
        clipSliceA_BG.setPickable(true);

        clipSliceAIndicator_BG = new BranchGroup();
        clipSliceAIndicator_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        clipSliceAIndicator_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        clipSliceAIndicator_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        clipSliceAIndicator_BG.setCapability(BranchGroup.ALLOW_DETACH);

        arbitrary_SG = new Switch();
        arbitrary_SG.addChild(clipSliceA_BG);
        arbitrary_SG.addChild(clipSliceAIndicator_BG);
        arbitrary_SG.setWhichChild(Switch.CHILD_NONE);
        arbitrary_SG.setCapability(Switch.ALLOW_SWITCH_WRITE);
        arbitrary_SG.setCapability(Switch.ALLOW_CHILDREN_WRITE);
        arbitrary_SG.setCapability(Switch.ALLOW_CHILDREN_READ);
        arbitrary_SG.setWhichChild(0);

        arbiTG = new TransformGroup();
        arbiTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        arbiTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        arbiTG.setCapability(Group.ALLOW_CHILDREN_READ);
        arbiTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        arbiTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        arbiTG.setCapability(TransformGroup.ENABLE_PICK_REPORTING);
        arbiTG.addChild(arbitrary_SG);
        objClipSliceA_BG.addChild(arbiTG);

        arbiTrans3d = new Transform3D();
        arbiTG.setTransform(arbiTrans3d);

        behaviorBG = new BranchGroup();
        behaviorBG.setCapability(BranchGroup.ALLOW_DETACH);
        behaviorBG.setCapability(Group.ALLOW_CHILDREN_READ);
        behaviorBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        behaviorBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        arbiMouseRotateBehavior = new MouseRotate(arbiTG);
        behaviorBG.addChild(arbiMouseRotateBehavior);
        arbiTG.addChild(behaviorBG);
        arbiMouseRotateBehavior.setupCallback(this);
        arbiMouseRotateBehavior.setSchedulingBounds(renderBase.getBound());
        arbiMouseRotateBehavior.setFactor(0.005);

        objClipSliceX_BG = new BranchGroup();
        objClipSliceX_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceX_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceX_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceX_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceX_BG.setPickable(false);

        objClipSliceY_BG = new BranchGroup();
        objClipSliceY_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceY_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceY_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceY_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceY_BG.setPickable(false);

        objClipSliceZ_BG = new BranchGroup();
        objClipSliceZ_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceZ_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceZ_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceZ_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceZ_BG.setPickable(false);

        objClipSliceStatic_BG = new BranchGroup();
        objClipSliceStatic_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceStatic_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceStatic_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceStatic_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceStatic_BG.setPickable(false);

        objClipSliceStaticInv_BG = new BranchGroup();
        objClipSliceStaticInv_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceStaticInv_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceStaticInv_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceStaticInv_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceStaticInv_BG.setPickable(false);

        objClipSliceXInv_BG = new BranchGroup();
        objClipSliceXInv_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceXInv_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceXInv_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceXInv_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceXInv_BG.setPickable(false);

        objClipSliceYInv_BG = new BranchGroup();
        objClipSliceYInv_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceYInv_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceYInv_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceYInv_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceYInv_BG.setPickable(false);

        objClipSliceZInv_BG = new BranchGroup();
        objClipSliceZInv_BG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        objClipSliceZInv_BG.setCapability(Group.ALLOW_CHILDREN_READ);
        objClipSliceZInv_BG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        objClipSliceZInv_BG.setCapability(BranchGroup.ALLOW_DETACH);
        objClipSliceZInv_BG.setPickable(false);

        clipSliceAIndicator = new ViewJComponentBoxSlice(1, yBox, zBox * radicalRatio,
                                                         ViewJComponentBoxSlice.A_CLIPSLICE);
        clipSliceAIndicator.setColor(new Color(0.94f, 0.67f, 0.69f));
        shape = new Shape3D(clipSliceAIndicator, null);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        shape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);
        clipSliceAIndicator_BG.addChild(shape);

        clipSliceA = new ViewJComponentBoxSlice(1, yBox * radicalRatio, zBox * radicalRatio,
                                                ViewJComponentBoxSlice.A_CLIPSLICE);
        shape = new Shape3D(clipSliceA, null);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        shape.setCapability(Shape3D.ALLOW_GEOMETRY_WRITE);
        shape.setCapability(Shape3D.ALLOW_GEOMETRY_READ);

        try {
            ((SurfaceRender) renderBase).getSurfaceDialog().getPickCanvas().setCapabilities(shape, PickTool.INTERSECT_FULL);
        } catch (RestrictedAccessException error) { }

        clipSliceA_BG.addChild(shape);

        clipSliceX = new ViewJComponentBoxSlice(1, yBox, zBox, ViewJComponentBoxSlice.X_CLIPSLICE);
        shape = new Shape3D(clipSliceX, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceX_BG.addChild(shape);

        clipSliceY = new ViewJComponentBoxSlice(xBox, -1, zBox, ViewJComponentBoxSlice.Y_CLIPSLICE);
        shape = new Shape3D(clipSliceY, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceY_BG.addChild(shape);

        clipSliceZ = new ViewJComponentBoxSlice(xBox, yBox, -zBox, ViewJComponentBoxSlice.Z_CLIPSLICE);
        shape = new Shape3D(clipSliceZ, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceZ_BG.addChild(shape);

        clipSliceStatic = new ViewJComponentBoxSlice(xBox * 0.5f, yBox * 0.5f, zBox,
                                                     ViewJComponentBoxSlice.S_CLIPSLICE);
        shape = new Shape3D(clipSliceStatic, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceStatic_BG.addChild(shape);

        clipSliceStaticInv = new ViewJComponentBoxSlice(xBox * 0.5f, yBox * 0.5f, zBox,
                                                        ViewJComponentBoxSlice.S_CLIPSLICE_NEG);
        shape = new Shape3D(clipSliceStaticInv, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceStaticInv_BG.addChild(shape);

        clipSliceXInv = new ViewJComponentBoxSlice(-1, yBox, zBox, ViewJComponentBoxSlice.X_CLIPSLICE_NEG);
        shape = new Shape3D(clipSliceXInv, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceXInv_BG.addChild(shape);

        clipSliceYInv = new ViewJComponentBoxSlice(xBox, 1, zBox, ViewJComponentBoxSlice.Y_CLIPSLICE_NEG);
        shape = new Shape3D(clipSliceYInv, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceYInv_BG.addChild(shape);

        clipSliceZInv = new ViewJComponentBoxSlice(xBox, yBox, zBox, ViewJComponentBoxSlice.Z_CLIPSLICE_NEG);
        shape = new Shape3D(clipSliceZInv, null);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
        objClipSliceZInv_BG.addChild(shape);

        mcBG = new BranchGroup();
        mcBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        mcBG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcBG.setCapability(BranchGroup.ALLOW_DETACH);

        mcStaticBG = new BranchGroup();
        mcStaticBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        mcStaticBG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcStaticBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcStaticBG.setCapability(BranchGroup.ALLOW_DETACH);

        mcArbiBG = new BranchGroup();
        mcArbiBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        mcArbiBG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcArbiBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcArbiBG.setCapability(BranchGroup.ALLOW_DETACH);

        mcExtendBG = new BranchGroup();
        mcExtendBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        mcExtendBG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcExtendBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcExtendBG.setCapability(BranchGroup.ALLOW_DETACH);

        mcArbiExtendBG = new BranchGroup();
        mcArbiExtendBG.setCapability(Group.ALLOW_CHILDREN_EXTEND);
        mcArbiExtendBG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcArbiExtendBG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcArbiExtendBG.setCapability(BranchGroup.ALLOW_DETACH);

        mcArbiTG = new TransformGroup();
        mcArbiTG.setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
        mcArbiTG.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
        mcArbiTG.setCapability(Group.ALLOW_CHILDREN_READ);
        mcArbiTG.setCapability(Group.ALLOW_CHILDREN_WRITE);
        mcArbiTG.setCapability(Group.ALLOW_CHILDREN_EXTEND);

        mcArbiTrans3D = new Transform3D();
        mcArbiTG.setTransform(mcArbiTrans3D);

        mc = new ModelClip();
        mc.setCapability(ModelClip.ALLOW_ENABLE_READ);
        mc.setCapability(ModelClip.ALLOW_ENABLE_WRITE);
        mc.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_READ);
        mc.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_WRITE);
        mc.setCapability(ModelClip.ALLOW_PLANE_READ);
        mc.setCapability(ModelClip.ALLOW_PLANE_WRITE);
        mc.setCapability(ModelClip.ALLOW_SCOPE_READ);
        mc.setCapability(ModelClip.ALLOW_SCOPE_WRITE);

        mcArbi = new ModelClip();
        mcArbi.setCapability(ModelClip.ALLOW_ENABLE_READ);
        mcArbi.setCapability(ModelClip.ALLOW_ENABLE_WRITE);
        mcArbi.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_READ);
        mcArbi.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_WRITE);
        mcArbi.setCapability(ModelClip.ALLOW_PLANE_READ);
        mcArbi.setCapability(ModelClip.ALLOW_PLANE_WRITE);
        mcArbi.setCapability(ModelClip.ALLOW_SCOPE_READ);
        mcArbi.setCapability(ModelClip.ALLOW_SCOPE_WRITE);

        mcStatic = new ModelClip();
        mcStatic.setCapability(ModelClip.ALLOW_ENABLE_READ);
        mcStatic.setCapability(ModelClip.ALLOW_ENABLE_WRITE);
        mcStatic.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_READ);
        mcStatic.setCapability(ModelClip.ALLOW_INFLUENCING_BOUNDS_WRITE);
        mcStatic.setCapability(ModelClip.ALLOW_PLANE_READ);
        mcStatic.setCapability(ModelClip.ALLOW_PLANE_WRITE);
        mcStatic.setCapability(ModelClip.ALLOW_SCOPE_READ);
        mcStatic.setCapability(ModelClip.ALLOW_SCOPE_WRITE);

        mc.setEnables(enables);
        mcArbi.setEnables(enablesArbi);
        mcStatic.setEnables(enablesStatic);

        mcExtendBG.addChild(mc);
        mcBG.addChild(mcExtendBG);
        ((SurfaceRender) renderBase).getVolRenderOG().insertChild(mcBG, 0);
        mcArbiBG.addChild(mcArbiTG);

        mcArbiExtendBG.addChild(mcArbi);
        mcArbiTG.addChild(mcArbiExtendBG);

        ((SurfaceRender) renderBase).getVolRenderOG().insertChild(mcArbiBG, 0);
        mc.addScope(((SurfaceRender) renderBase).getVolRenderBG());
        mcArbi.addScope(((SurfaceRender) renderBase).getVolRenderBG());

        mcStaticBG.addChild(mcStatic);
        renderBase.getBranchGroup().addChild(mcStaticBG);
        mcStatic.addScope(((SurfaceRender) renderBase).getVolRenderBG());

        isFirstTimeBuildTree = false;
    }

    /**
     * Build the arbitrary clipping slider control panel.
     */
    public void buildPanelA() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelA = new JPanel();
        panelA.setBounds(10, 100, 100, 120);
        panelA.setLayout(cpGBL);

        boxA = new JCheckBox();
        boxA.setSelected(false);
        boxA.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxA.addActionListener(this);
        boxA.setActionCommand("A");
        boxA.setText("A Slider");
        boxA.setFont(serif12);
        addControlPanel(panelA, boxA, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelA = new JLabel(" A (" + String.valueOf(-xDim) + " - " + String.valueOf((int) (xDim * radicalRatio)) + ")");
        labelA.setForeground(Color.black);
        labelA.setFont(MipavUtil.font12);
        labelA.setEnabled(false);
        addControlPanel(panelA, labelA, cpGBC, 0, 1, 2, 1);

        sliderA = new JSlider(-xDim, (int) (xDim * radicalRatio), (int) (xDim * radicalRatio));
        sliderA.setFont(MipavUtil.font12);
        sliderA.setEnabled(false);
        sliderA.setMinorTickSpacing(xDim / 10);
        sliderA.setPaintTicks(true);
        sliderA.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        sliderA.addChangeListener(this);
        sliderA.addMouseListener(this);
        aSlice = sliderA.getValue();
        sliderA.setVisible(true);

        labelAStart = new JLabel(String.valueOf(-xDim));
        labelAStart.setForeground(Color.black);
        labelAStart.setFont(MipavUtil.font12);
        labelAStart.setEnabled(false);
        labelAMid = new JLabel(String.valueOf((int) ((((xDim * radicalRatio) + xDim) / radicalRatio) - xDim)));
        labelAMid.setForeground(Color.black);
        labelAMid.setFont(MipavUtil.font12);
        labelAMid.setEnabled(false);
        labelAEnd = new JLabel(String.valueOf((int) (xDim * radicalRatio)));
        labelAEnd.setForeground(Color.black);
        labelAEnd.setFont(MipavUtil.font12);
        labelAEnd.setEnabled(false);

        Hashtable labelTableA = new Hashtable();

        labelTableA.put(new Integer(-xDim), labelAStart);
        labelTableA.put(new Integer((int) ((((xDim * radicalRatio) + xDim) / radicalRatio) - xDim)), labelAMid);
        labelTableA.put(new Integer((int) (xDim * radicalRatio)), labelAEnd);
        sliderA.setLabelTable(labelTableA);
        sliderA.setPaintLabels(true);
        addControlPanel(panelA, sliderA, cpGBC, 2, 1, 8, 1);

        textA = new JTextField(String.valueOf((int) (xDim * radicalRatio)), 4);
        textA.setFont(MipavUtil.font12);
        textA.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelA, textA, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckA = new JCheckBox("A Frame");
        boundingCheckA.setFont(serif12);
        boundingCheckA.addActionListener(this);
        addControlPanel(panelA, boundingCheckA, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonA = new JButton();
        colorButtonA.setPreferredSize(new Dimension(25, 25));
        colorButtonA.setToolTipText("Change arbitrary clipping plane frame color");
        colorButtonA.addActionListener(this);
        colorButtonA.setBackground(new Color(0.73f, 0.70f, 0.86f)); // light blue
        colorButtonA.setEnabled(false);

        extractButtonA = new JButton(MipavUtil.getIcon("extract.gif"));
        extractButtonA.addActionListener(this);
        extractButtonA.setToolTipText("Extract Slice");
        extractButtonA.setActionCommand("extract");
        extractButtonA.setBorderPainted(false);
        extractButtonA.setRolloverEnabled(true);
        extractButtonA.setRolloverIcon(MipavUtil.getIcon("extractroll.gif"));
        extractButtonA.setFocusPainted(false);
        extractButtonA.setEnabled(true);

        buttonPanel.add(colorButtonA);
        buttonPanel.add(extractButtonA);

        addControlPanel(panelA, buttonPanel, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("A", null, panelA);
    }

    /**
     * Build static clipping slider control panel.
     */
    public void buildPanelS() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelS = new JPanel();
        panelS.setBounds(10, 100, 100, 120);
        panelS.setLayout(cpGBL);

        // Build static clipping plane
        boxStatic = new JCheckBox();
        boxStatic.setSelected(false);
        boxStatic.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxStatic.addActionListener(this);
        boxStatic.setActionCommand("EYE");
        boxStatic.setText("Eye Slider");
        boxStatic.setFont(serif12);
        addControlPanel(panelS, boxStatic, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelStatic = new JLabel(" EYE (1 - " + String.valueOf(zDim) + ")");
        labelStatic.setForeground(Color.black);
        labelStatic.setFont(MipavUtil.font12);
        labelStatic.setEnabled(false);
        addControlPanel(panelS, labelStatic, cpGBC, 0, 1, 2, 1);

        clipSliderStatic = new JSlider(1, zDim, 1);
        clipSliderStatic.setFont(MipavUtil.font12);
        clipSliderStatic.setEnabled(false);
        clipSliderStatic.setMinorTickSpacing(zDim / 10);
        clipSliderStatic.setPaintTicks(true);
        clipSliderStatic.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        clipSliderStatic.addChangeListener(this);
        clipSliderStatic.addMouseListener(this);
        sSlice = clipSliderStatic.getValue();
        clipSliderStatic.setVisible(true);
        clipSliderStatic.setEnabled(false);

        labelStaticStart = new JLabel("1");
        labelStaticStart.setForeground(Color.black);
        labelStaticStart.setFont(MipavUtil.font12);
        labelStaticStart.setEnabled(false);
        labelStaticMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelStaticMid.setForeground(Color.black);
        labelStaticMid.setFont(MipavUtil.font12);
        labelStaticMid.setEnabled(false);
        labelStaticEnd = new JLabel(String.valueOf(zDim));
        labelStaticEnd.setForeground(Color.black);
        labelStaticEnd.setFont(MipavUtil.font12);
        labelStaticEnd.setEnabled(false);

        Hashtable labelTableStatic = new Hashtable();

        labelTableStatic.put(new Integer(1), labelStaticStart);
        labelTableStatic.put(new Integer((zDim + 1) / 2), labelStaticMid);
        labelTableStatic.put(new Integer(zDim), labelStaticEnd);
        clipSliderStatic.setLabelTable(labelTableStatic);
        clipSliderStatic.setPaintLabels(true);
        addControlPanel(panelS, clipSliderStatic, cpGBC, 2, 1, 8, 1);

        textStatic = new JTextField(String.valueOf(1), 4);
        textStatic.setFont(MipavUtil.font12);
        textStatic.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelS, textStatic, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckStatic = new JCheckBox("Eye Frame");
        boundingCheckStatic.setFont(serif12);
        boundingCheckStatic.addActionListener(this);
        addControlPanel(panelS, boundingCheckStatic, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonStatic = new JButton();
        colorButtonStatic.setPreferredSize(new Dimension(25, 25));
        colorButtonStatic.setToolTipText("Change eye clipping plane frame color");
        colorButtonStatic.addActionListener(this);
        colorButtonStatic.setBackground(Color.orange);
        colorButtonStatic.setEnabled(false);

        extractButtonS = new JButton(MipavUtil.getIcon("extract.gif"));
        extractButtonS.addActionListener(this);
        extractButtonS.setToolTipText("Extract Slice");
        extractButtonS.setActionCommand("extract");
        extractButtonS.setBorderPainted(false);
        extractButtonS.setRolloverEnabled(true);
        extractButtonS.setRolloverIcon(MipavUtil.getIcon("extractroll.gif"));
        extractButtonS.setFocusPainted(false);
        extractButtonS.setEnabled(true);

        buttonPanel.add(colorButtonStatic);
        buttonPanel.add(extractButtonS);

        addControlPanel(panelS, buttonPanel, cpGBC, 1, 2, 2, 1);

        tabbedPane.addTab("EYE", null, panelS);
    }

    /**
     * Build static inverse clipping slider control panel.
     */
    public void buildPanelSInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelSInv = new JPanel();
        panelSInv.setBounds(10, 100, 100, 120);
        panelSInv.setLayout(cpGBL);

        // Build static clipping plane
        boxStaticInv = new JCheckBox();
        boxStaticInv.setSelected(false);
        boxStaticInv.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxStaticInv.addActionListener(this);
        boxStaticInv.setActionCommand("-EYE");
        boxStaticInv.setText("Eye Slider");
        boxStaticInv.setFont(serif12);
        addControlPanel(panelSInv, boxStaticInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelStaticInv = new JLabel(" -EYE (1 - " + String.valueOf(zDim) + ")");
        labelStaticInv.setForeground(Color.black);
        labelStaticInv.setFont(MipavUtil.font12);
        labelStaticInv.setEnabled(false);
        addControlPanel(panelSInv, labelStaticInv, cpGBC, 0, 1, 2, 1);

        clipSliderStaticInv = new JSlider(1, zDim, zDim);
        clipSliderStaticInv.setFont(MipavUtil.font12);
        clipSliderStaticInv.setEnabled(false);
        clipSliderStaticInv.setMinorTickSpacing(zDim / 10);
        clipSliderStaticInv.setPaintTicks(true);
        clipSliderStaticInv.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        clipSliderStaticInv.addChangeListener(this);
        clipSliderStaticInv.addMouseListener(this);
        sSliceInv = clipSliderStaticInv.getValue();
        clipSliderStaticInv.setVisible(true);
        clipSliderStaticInv.setEnabled(false);

        labelStaticInvStart = new JLabel("1");
        labelStaticInvStart.setForeground(Color.black);
        labelStaticInvStart.setFont(MipavUtil.font12);
        labelStaticInvStart.setEnabled(false);
        labelStaticInvMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelStaticInvMid.setForeground(Color.black);
        labelStaticInvMid.setFont(MipavUtil.font12);
        labelStaticInvMid.setEnabled(false);
        labelStaticInvEnd = new JLabel(String.valueOf(zDim));
        labelStaticInvEnd.setForeground(Color.black);
        labelStaticInvEnd.setFont(MipavUtil.font12);
        labelStaticInvEnd.setEnabled(false);

        Hashtable labelTableStatic = new Hashtable();

        labelTableStatic.put(new Integer(1), labelStaticStart);
        labelTableStatic.put(new Integer((zDim + 1) / 2), labelStaticMid);
        labelTableStatic.put(new Integer(zDim), labelStaticEnd);
        clipSliderStaticInv.setLabelTable(labelTableStatic);
        clipSliderStaticInv.setPaintLabels(true);
        addControlPanel(panelSInv, clipSliderStaticInv, cpGBC, 2, 1, 8, 1);

        textStaticInv = new JTextField(String.valueOf(1), 4);
        textStaticInv.setFont(MipavUtil.font12);
        textStaticInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelSInv, textStaticInv, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckStaticInv = new JCheckBox("Eye Frame");
        boundingCheckStaticInv.setFont(serif12);
        boundingCheckStaticInv.addActionListener(this);
        addControlPanel(panelSInv, boundingCheckStaticInv, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonStaticInv = new JButton();
        colorButtonStaticInv.setPreferredSize(new Dimension(25, 25));
        colorButtonStaticInv.setToolTipText("Change eye clipping plane frame color");
        colorButtonStaticInv.addActionListener(this);
        colorButtonStaticInv.setBackground(new Color(1.0f, 0.64f, 1.0f));
        colorButtonStaticInv.setEnabled(false);

        buttonPanel.add(colorButtonStaticInv);

        addControlPanel(panelSInv, buttonPanel, cpGBC, 1, 2, 2, 1);

        tabbedPane.addTab("-EYE", null, panelSInv);
    }

    /**
     * Build x slider control panel.
     */
    public void buildPanelX() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelX = new JPanel();
        panelX.setBounds(10, 100, 100, 120);
        panelX.setLayout(cpGBL);

        boxX = new JCheckBox();
        boxX.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxX.addActionListener(this);
        boxX.setActionCommand("+X");
        boxX.setText("X Slider");
        boxX.setFont(serif12);
        addControlPanel(panelX, boxX, cpGBC, 0, 0, 1, 1);
        boxX.setSelected(false);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelX = new JLabel(" +X (1 - " + String.valueOf(xDim) + ")");
        labelX.setForeground(Color.black);
        labelX.setFont(MipavUtil.font12);
        addControlPanel(panelX, labelX, cpGBC, 0, 1, 2, 1);
        labelX.setEnabled(false);

        clipSliderX = new JSlider(1, xDim, xDim);
        clipSliderX.setFont(MipavUtil.font12);
        clipSliderX.setMinorTickSpacing(xDim / 10);
        clipSliderX.setPaintTicks(true);
        clipSliderX.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        clipSliderX.addChangeListener(this);
        clipSliderX.addMouseListener(this);
        xSlice = clipSliderX.getValue() - 1;
        clipSliderX.setVisible(true);
        clipSliderX.setEnabled(false);

        labelXStart = new JLabel("1");
        labelXStart.setForeground(Color.black);
        labelXStart.setFont(MipavUtil.font12);
        labelXStart.setEnabled(false);
        labelXMid = new JLabel(String.valueOf((xDim + 1) / 2));
        labelXMid.setForeground(Color.black);
        labelXMid.setFont(MipavUtil.font12);
        labelXMid.setEnabled(false);
        labelXEnd = new JLabel(String.valueOf(xDim));
        labelXEnd.setForeground(Color.black);
        labelXEnd.setFont(MipavUtil.font12);
        labelXEnd.setEnabled(false);

        Hashtable labelTableX = new Hashtable();

        labelTableX.put(new Integer(1), labelXStart);
        labelTableX.put(new Integer((xDim + 1) / 2), labelXMid);
        labelTableX.put(new Integer(xDim), labelXEnd);
        clipSliderX.setLabelTable(labelTableX);
        clipSliderX.setPaintLabels(true);
        addControlPanel(panelX, clipSliderX, cpGBC, 2, 1, 8, 1);

        textX = new JTextField(String.valueOf(xDim), 4);
        textX.setFont(MipavUtil.font12);
        textX.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelX, textX, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckX = new JCheckBox("X Frame");
        boundingCheckX.setFont(serif12);
        boundingCheckX.addActionListener(this);
        addControlPanel(panelX, boundingCheckX, cpGBC, 0, 2, 1, 1);

        colorButtonX = new JButton();
        colorButtonX.setPreferredSize(new Dimension(25, 25));
        colorButtonX.setToolTipText("Change +x clip frame color");
        colorButtonX.addActionListener(this);
        colorButtonX.setBackground(Color.yellow);
        colorButtonX.setEnabled(false);
        addControlPanel(panelX, colorButtonX, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("X", null, panelX);
    }

    /**
     * Build x negative clipping slider control panel.
     */
    public void buildPanelXInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelXInv = new JPanel();
        panelXInv.setBounds(10, 100, 100, 120);
        panelXInv.setLayout(cpGBL);

        boxXInv = new JCheckBox();
        boxXInv.setSelected(false);
        boxXInv.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxXInv.addActionListener(this);
        boxXInv.setActionCommand("-X");
        boxXInv.setText("-X Slider");
        boxXInv.setFont(serif12);
        addControlPanel(panelXInv, boxXInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelXInv = new JLabel(" -X (1 - " + String.valueOf(xDim) + ")");
        labelXInv.setForeground(Color.black);
        labelXInv.setFont(MipavUtil.font12);
        labelXInv.setEnabled(false);
        addControlPanel(panelXInv, labelXInv, cpGBC, 0, 1, 2, 1);

        sliderXInv = new JSlider(1, xDim, 1);
        sliderXInv.setFont(MipavUtil.font12);
        sliderXInv.setEnabled(false);
        sliderXInv.setMinorTickSpacing(xDim / 10);
        sliderXInv.setPaintTicks(true);
        sliderXInv.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        sliderXInv.addChangeListener(this);
        sliderXInv.addMouseListener(this);
        xSliceInv = sliderXInv.getValue() - 1;
        sliderXInv.setVisible(true);
        sliderXInv.setEnabled(false);

        labelXStartInv = new JLabel("1");
        labelXStartInv.setForeground(Color.black);
        labelXStartInv.setFont(MipavUtil.font12);
        labelXStartInv.setEnabled(false);
        labelXMidInv = new JLabel(String.valueOf((xDim + 1) / 2));
        labelXMidInv.setForeground(Color.black);
        labelXMidInv.setFont(MipavUtil.font12);
        labelXMidInv.setEnabled(false);
        labelXEndInv = new JLabel(String.valueOf(xDim));
        labelXEndInv.setForeground(Color.black);
        labelXEndInv.setFont(MipavUtil.font12);
        labelXEndInv.setEnabled(false);

        Hashtable labelTableXInv = new Hashtable();

        labelTableXInv.put(new Integer(1), labelXStartInv);
        labelTableXInv.put(new Integer((xDim + 1) / 2), labelXMidInv);
        labelTableXInv.put(new Integer(xDim), labelXEndInv);
        sliderXInv.setLabelTable(labelTableXInv);
        sliderXInv.setPaintLabels(true);
        addControlPanel(panelXInv, sliderXInv, cpGBC, 1, 1, 8, 1);

        textXInv = new JTextField(String.valueOf(1), 4);
        textXInv.setFont(MipavUtil.font12);
        textXInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelXInv, textXInv, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckXInv = new JCheckBox("-X Frame");
        boundingCheckXInv.setFont(serif12);
        boundingCheckXInv.addActionListener(this);
        addControlPanel(panelXInv, boundingCheckXInv, cpGBC, 0, 2, 1, 1);

        colorButtonXInv = new JButton();
        colorButtonXInv.setPreferredSize(new Dimension(25, 25));
        colorButtonXInv.setToolTipText("Change -x clip frame color");
        colorButtonXInv.addActionListener(this);
        colorButtonXInv.setBackground(Color.yellow);
        colorButtonXInv.setEnabled(false);
        addControlPanel(panelXInv, colorButtonXInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-X", null, panelXInv);
    }

    /**
     * Build the y clipping slider control panel.
     */
    public void buildPanelY() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelY = new JPanel();
        panelY.setBounds(10, 100, 100, 120);
        panelY.setLayout(cpGBL);

        boxY = new JCheckBox();
        boxY.setSelected(false);
        boxY.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxY.addActionListener(this);
        boxY.setActionCommand("+Y");
        boxY.setText("Y Slider");
        boxY.setFont(serif12);
        addControlPanel(panelY, boxY, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelY = new JLabel(" +Y (1 - " + String.valueOf(yDim) + ")");
        labelY.setForeground(Color.black);
        labelY.setFont(MipavUtil.font12);
        labelY.setEnabled(false);
        addControlPanel(panelY, labelY, cpGBC, 0, 1, 2, 1);

        clipSliderY = new JSlider(1, yDim, yDim);
        clipSliderY.setFont(MipavUtil.font12);
        clipSliderY.setEnabled(false);
        clipSliderY.setMinorTickSpacing(yDim / 10);
        clipSliderY.setPaintTicks(true);
        clipSliderY.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        clipSliderY.addChangeListener(this);
        clipSliderY.addMouseListener(this);
        ySlice = clipSliderY.getValue() - 1;
        clipSliderY.setVisible(true);
        clipSliderY.setEnabled(false);

        labelYStart = new JLabel("1");
        labelYStart.setForeground(Color.black);
        labelYStart.setFont(MipavUtil.font12);
        labelYStart.setEnabled(false);
        labelYMid = new JLabel(String.valueOf((yDim + 1) / 2));
        labelYMid.setForeground(Color.black);
        labelYMid.setFont(MipavUtil.font12);
        labelYMid.setEnabled(false);
        labelYEnd = new JLabel(String.valueOf(yDim));
        labelYEnd.setForeground(Color.black);
        labelYEnd.setFont(MipavUtil.font12);
        labelYEnd.setEnabled(false);

        Hashtable labelTableY = new Hashtable();

        labelTableY.put(new Integer(1), labelYStart);
        labelTableY.put(new Integer((yDim + 1) / 2), labelYMid);
        labelTableY.put(new Integer(yDim), labelYEnd);
        clipSliderY.setLabelTable(labelTableY);
        clipSliderY.setPaintLabels(true);
        addControlPanel(panelY, clipSliderY, cpGBC, 2, 1, 8, 1);

        textY = new JTextField(String.valueOf(yDim), 4);
        textY.setFont(MipavUtil.font12);
        textY.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelY, textY, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckY = new JCheckBox("Y Frame");
        boundingCheckY.setFont(serif12);
        boundingCheckY.addActionListener(this);
        addControlPanel(panelY, boundingCheckY, cpGBC, 0, 2, 1, 1);

        colorButtonY = new JButton();
        colorButtonY.setPreferredSize(new Dimension(25, 25));
        colorButtonY.setToolTipText("Change +y clipping plane frame color");
        colorButtonY.addActionListener(this);
        colorButtonY.setBackground(Color.green);
        colorButtonY.setEnabled(false);
        addControlPanel(panelY, colorButtonY, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("Y", null, panelY);
    }

    /**
     * Build the y negative clipping slider control panel.
     */
    public void buildPanelYInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelYInv = new JPanel();
        panelYInv.setBounds(10, 100, 100, 120);
        panelYInv.setLayout(cpGBL);

        boxYInv = new JCheckBox();
        boxYInv.setSelected(false);
        boxYInv.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxYInv.addActionListener(this);
        boxYInv.setActionCommand("-Y");
        boxYInv.setText("-Y Slider");
        boxYInv.setFont(serif12);
        addControlPanel(panelYInv, boxYInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelYInv = new JLabel(" -Y (1 - " + String.valueOf(yDim) + ")");
        labelYInv.setForeground(Color.black);
        labelYInv.setFont(MipavUtil.font12);
        labelYInv.setEnabled(false);
        addControlPanel(panelYInv, labelYInv, cpGBC, 0, 1, 2, 1);

        sliderYInv = new JSlider(1, yDim, 1);
        sliderYInv.setFont(MipavUtil.font12);
        sliderYInv.setEnabled(false);
        sliderYInv.setMinorTickSpacing(yDim / 10);
        sliderYInv.setPaintTicks(true);
        sliderYInv.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        sliderYInv.addChangeListener(this);
        sliderYInv.addMouseListener(this);
        ySliceInv = sliderYInv.getValue() - 1;
        sliderYInv.setVisible(true);
        sliderYInv.setEnabled(false);

        labelYStartInv = new JLabel("1");
        labelYStartInv.setForeground(Color.black);
        labelYStartInv.setFont(MipavUtil.font12);
        labelYStartInv.setEnabled(false);
        labelYMidInv = new JLabel(String.valueOf((yDim + 1) / 2));
        labelYMidInv.setForeground(Color.black);
        labelYMidInv.setFont(MipavUtil.font12);
        labelYMidInv.setEnabled(false);
        labelYEndInv = new JLabel(String.valueOf(yDim));
        labelYEndInv.setForeground(Color.black);
        labelYEndInv.setFont(MipavUtil.font12);
        labelYEndInv.setEnabled(false);

        Hashtable labelTableYInv = new Hashtable();

        labelTableYInv.put(new Integer(1), labelYStartInv);
        labelTableYInv.put(new Integer((yDim + 1) / 2), labelYMidInv);
        labelTableYInv.put(new Integer(yDim), labelYEndInv);
        sliderYInv.setLabelTable(labelTableYInv);
        sliderYInv.setPaintLabels(true);
        addControlPanel(panelYInv, sliderYInv, cpGBC, 2, 1, 8, 1);

        textYInv = new JTextField(String.valueOf(1), 4);
        textYInv.setFont(MipavUtil.font12);
        textYInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelYInv, textYInv, cpGBC, 10, 1, 1, 1);

        // add bounnding box
        boundingCheckYInv = new JCheckBox("-Y Frame");
        boundingCheckYInv.setFont(serif12);
        boundingCheckYInv.addActionListener(this);
        addControlPanel(panelYInv, boundingCheckYInv, cpGBC, 0, 2, 1, 1);

        colorButtonYInv = new JButton();
        colorButtonYInv.setPreferredSize(new Dimension(25, 25));
        colorButtonYInv.setToolTipText("Change -y clipping plane frame color");
        colorButtonYInv.addActionListener(this);
        colorButtonYInv.setBackground(Color.green);
        colorButtonYInv.setEnabled(false);
        addControlPanel(panelYInv, colorButtonYInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-Y", null, panelYInv);
    }

    /**
     * Build the z clipping slider control panel.
     */
    public void buildPanelZ() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelZ = new JPanel();
        panelZ.setBounds(10, 100, 100, 120);
        panelZ.setLayout(cpGBL);

        boxZ = new JCheckBox();
        boxZ.setSelected(false);
        boxZ.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxZ.addActionListener(this);
        boxZ.setActionCommand("+Z");
        boxZ.setText("Z Slider");
        boxZ.setFont(serif12);
        addControlPanel(panelZ, boxZ, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelZ = new JLabel(" +Z (1 - " + String.valueOf(zDim) + ")");
        labelZ.setForeground(Color.black);
        labelZ.setFont(MipavUtil.font12);
        labelZ.setEnabled(false);
        addControlPanel(panelZ, labelZ, cpGBC, 0, 1, 2, 1);

        clipSliderZ = new JSlider(1, zDim, zDim);
        clipSliderZ.setFont(MipavUtil.font12);
        clipSliderZ.setEnabled(false);
        clipSliderZ.setMinorTickSpacing(zDim / 10);
        clipSliderZ.setPaintTicks(true);
        clipSliderZ.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        clipSliderZ.addChangeListener(this);
        clipSliderZ.addMouseListener(this);
        zSlice = clipSliderZ.getValue() - 1;
        clipSliderZ.setVisible(true);
        clipSliderZ.setEnabled(false);

        labelZStart = new JLabel("1");
        labelZStart.setForeground(Color.black);
        labelZStart.setFont(MipavUtil.font12);
        labelZStart.setEnabled(false);
        labelZMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelZMid.setForeground(Color.black);
        labelZMid.setFont(MipavUtil.font12);
        labelZMid.setEnabled(false);
        labelZEnd = new JLabel(String.valueOf(zDim));
        labelZEnd.setForeground(Color.black);
        labelZEnd.setFont(MipavUtil.font12);
        labelZEnd.setEnabled(false);

        Hashtable labelTableZ = new Hashtable();

        labelTableZ.put(new Integer(1), labelZStart);
        labelTableZ.put(new Integer((zDim + 1) / 2), labelZMid);
        labelTableZ.put(new Integer(zDim), labelZEnd);
        clipSliderZ.setLabelTable(labelTableZ);
        clipSliderZ.setPaintLabels(true);
        addControlPanel(panelZ, clipSliderZ, cpGBC, 2, 1, 8, 1);

        textZ = new JTextField(String.valueOf(zDim), 4);
        textZ.setFont(MipavUtil.font12);
        textZ.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelZ, textZ, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckZ = new JCheckBox("Z Frame");
        boundingCheckZ.setFont(serif12);
        boundingCheckZ.addActionListener(this);
        addControlPanel(panelZ, boundingCheckZ, cpGBC, 0, 2, 1, 1);

        colorButtonZ = new JButton();
        colorButtonZ.setPreferredSize(new Dimension(25, 25));
        colorButtonZ.setToolTipText("Change +z clipping plane frame color");
        colorButtonZ.addActionListener(this);
        colorButtonZ.setBackground(Color.red);
        colorButtonZ.setEnabled(false);
        addControlPanel(panelZ, colorButtonZ, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("Z", null, panelZ);
    }

    /**
     * Build the z negative clipping slider control panel.
     */
    public void buildPanelZInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelZInv = new JPanel();
        panelZInv.setBounds(10, 100, 100, 120);
        panelZInv.setLayout(cpGBL);

        boxZInv = new JCheckBox();
        boxZInv.setSelected(false);
        boxZInv.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        boxZInv.addActionListener(this);
        boxZInv.setActionCommand("-Z");
        boxZInv.setText("-Z Slider");
        boxZInv.setFont(serif12);
        addControlPanel(panelZInv, boxZInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelZInv = new JLabel(" -Z (1 - " + String.valueOf(zDim) + ")");
        labelZInv.setForeground(Color.black);
        labelZInv.setFont(MipavUtil.font12);
        labelZInv.setEnabled(false);
        addControlPanel(panelZInv, labelZInv, cpGBC, 0, 1, 2, 1);

        sliderZInv = new JSlider(1, zDim, 1);
        sliderZInv.setFont(MipavUtil.font12);
        sliderZInv.setEnabled(false);
        sliderZInv.setMinorTickSpacing(zDim / 10);
        sliderZInv.setPaintTicks(true);
        sliderZInv.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        sliderZInv.addChangeListener(this);
        sliderZInv.addMouseListener(this);
        zSliceInv = sliderZInv.getValue() -1;
        sliderZInv.setVisible(true);
        sliderZInv.setEnabled(false);

        labelZStartInv = new JLabel("1");
        labelZStartInv.setForeground(Color.black);
        labelZStartInv.setFont(MipavUtil.font12);
        labelZStartInv.setEnabled(false);
        labelZMidInv = new JLabel(String.valueOf((zDim + 1) / 2));
        labelZMidInv.setForeground(Color.black);
        labelZMidInv.setFont(MipavUtil.font12);
        labelZMidInv.setEnabled(false);
        labelZEndInv = new JLabel(String.valueOf(zDim));
        labelZEndInv.setForeground(Color.black);
        labelZEndInv.setFont(MipavUtil.font12);
        labelZEndInv.setEnabled(false);

        Hashtable labelTableZInv = new Hashtable();

        labelTableZInv.put(new Integer(1), labelZStartInv);
        labelTableZInv.put(new Integer((zDim + 1) / 2), labelZMidInv);
        labelTableZInv.put(new Integer(zDim), labelZEndInv);
        sliderZInv.setLabelTable(labelTableZInv);
        sliderZInv.setPaintLabels(true);
        addControlPanel(panelZInv, sliderZInv, cpGBC, 2, 1, 8, 1);

        textZInv = new JTextField(String.valueOf(1), 4);
        textZInv.setFont(MipavUtil.font12);
        textZInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelZInv, textZInv, cpGBC, 10, 1, 1, 1);

        // add buonding box
        boundingCheckZInv = new JCheckBox("-Z Frame");
        boundingCheckZInv.setFont(serif12);
        boundingCheckZInv.addActionListener(this);
        addControlPanel(panelZInv, boundingCheckZInv, cpGBC, 0, 2, 1, 1);

        colorButtonZInv = new JButton();
        colorButtonZInv.setPreferredSize(new Dimension(25, 25));
        colorButtonZInv.setToolTipText("Change -z clipping plane frame color");
        colorButtonZInv.addActionListener(this);
        colorButtonZInv.setBackground(Color.red);
        colorButtonZInv.setEnabled(false);
        addControlPanel(panelZInv, colorButtonZInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-Z", null, panelZInv);
    }

    /**
     * Mask the clipped volume region with image min intensity value.
     */
    public void cropVolume() {
        int iX, iY, iZ;
        double x = 0, xInv = 0;
        double y = 0, yInv = 0;
        double z = 0, zInv = 0;

        /* The plane coordinate x,y dimensions: */
        float m_fX0, m_fY0, m_fZ0, m_fX1, m_fY1, m_fZ1;

        int iXBound = xDim;
        int iYBound = yDim;
        int iZBound = zDim;
        int iSliceSize = iXBound * iYBound;
        float xB, yB, zB, maxB;

        xB = xBox;
        yB = yBox;
        zB = zBox;
        maxB = xBox;

        if (yB > maxB) {
            maxB = yB;
        }

        if (zB > maxB) {
            maxB = zB;
        }

        // Normalize the size
        // xBox range between 0 - 1.
        xB = xB / maxB;
        yB = yBox / maxB;
        zB = zBox / maxB;

        m_fX0 = -xB;
        m_fY0 = -yB;
        m_fX1 = xB;
        m_fY1 = yB;
        m_fZ0 = -zB;
        m_fZ1 = zB;

        maxB = xB;

        if (yB > maxB) {
            maxB = yB;
        }

        if (zB > maxB) {
            maxB = zB;
        }

        if (zB > maxB) {
            m_fZ0 = -1f;
            m_fZ1 = 1f;
        }

        eqnX = new Vector4d(1.0, 0.0, 0.0, xBox - (2 * ((float) xSlice / (xDim - 1)) * xBox) - 0.001);
        eqnXInv = new Vector4d(-1.0, 0.0, 0.0, -xBox + (2 * ((float) xSliceInv / (xDim - 1)) * xBox) - 0.001);
        eqnY = new Vector4d(0.0, -1.0, 0.0, yBox - (2 * ((float) ySlice / (yDim - 1)) * yBox) - 0.001);
        eqnYInv = new Vector4d(0.0, 1.0, 0.0, -yBox + (2 * ((float) ySliceInv / (yDim - 1)) * yBox) - 0.001);
        eqnZ = new Vector4d(0.0, 0.0, -1.0, zBox - (2 * ((float) sSliceInv / (zDim - 1)) * zBox) - 0.001);
        eqnZInv = new Vector4d(0.0, 0.0, 1.0, (2 * ((float) sSlice / (zDim - 1)) * zBox) - zBox - 0.001);
        updateClipPlanesEqn();

        x = eqnX.w;
        xInv = eqnXInv.w;
        y = eqnY.w;
        yInv = eqnYInv.w;
        z = eqnZ.w;
        zInv = eqnZInv.w;

        // transform the volume coordinate to image coordinate.
        xInv = ((xInv - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
        x = ((x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
        yInv = ((yInv - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
        y = ((y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
        zInv = ((zInv - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);
        z = ((z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

        ModelImage imgA = ((SurfaceRender) renderBase).getImageA();
        double minA = imgA.getMin();
        ModelImage imgB = ((SurfaceRender) renderBase).getImageB();
        double minB = imgA.getMin();

        for (iZ = 0; iZ < iZBound; iZ++) {

            for (iX = 0; iX < iXBound; iX++) {

                for (iY = 0; iY < iYBound; iY++) {
                    int iIndex = (int) (iZ * iSliceSize) + (iY * iXBound) + iX;
                    
                    if (((iX >= 0) && (iX < xInv)) || ((iX < iXBound) && (iX >= (iXBound - x)))) {
                    	
                    	if (imgA.isColorImage()) {
							for (int iColor = 0; iColor < 4; iColor++) {
								imgA.set((iIndex * 4) + iColor, minA);
							}
						} else {
							imgA.set(iIndex, minA);
						}

						if (imgB != null) {

							if (imgB.isColorImage()) {

								for (int iColor = 0; iColor < 4; iColor++) {
									imgB.set((iIndex * 4) + iColor, minB);
								}
							} else {
								imgB.set(iIndex, minB);
							}
						}
                    }

                    if (((iY >= 0) && (iY < yInv)) || ((iY < iYBound) && (iY >= (iYBound - y)))) {
                    	if (imgA.isColorImage()) {
							for (int iColor = 0; iColor < 4; iColor++) {
								imgA.set((iIndex * 4) + iColor, minA);
							}
						} else {
							imgA.set(iIndex, minA);
						}

						if (imgB != null) {

							if (imgB.isColorImage()) {

								for (int iColor = 0; iColor < 4; iColor++) {
									imgB.set((iIndex * 4) + iColor, minB);
								}
							} else {
								imgB.set(iIndex, minB);
							}
						}
                    }

                    if (((iZ >= 0) && (iZ < zInv)) || ((iZ < iZBound) && (iZ >= (iZBound - z)))) {
                    	if (imgA.isColorImage()) {
							for (int iColor = 0; iColor < 4; iColor++) {
								imgA.set((iIndex * 4) + iColor, minA);
							}
						} else {
							imgA.set(iIndex, minA);
						}

						if (imgB != null) {

							if (imgB.isColorImage()) {

								for (int iColor = 0; iColor < 4; iColor++) {
									imgB.set((iIndex * 4) + iColor, minB);
								}
							} else {
								imgB.set(iIndex, minB);
							}
						}
                    }
                }
            }
        }
        
        

        ((SurfaceRender) renderBase).updateData();
        ((SurfaceRender) renderBase).updateImages();
    }

    /**
     * Disable the 6 clipping planes.
     */
    public void disable6Planes() {
        setXSliderEnabled(false);
        boxX.setSelected(false);
        removeClipSliceX();
        boundingCheckX.setSelected(false);
        colorButtonX.setEnabled(false);

        setYSliderEnabled(false);
        boxY.setSelected(false);
        colorButtonY.setEnabled(false);
        removeClipSliceY();
        boundingCheckY.setSelected(false);

        setZSliderEnabled(false);
        boxZ.setSelected(false);
        colorButtonZ.setEnabled(false);
        removeClipSliceZ();
        boundingCheckZ.setSelected(false);

        setXSliderInvEnabled(false);
        boxXInv.setSelected(false);
        colorButtonXInv.setEnabled(false);
        removeClipSliceXInv();
        boundingCheckXInv.setSelected(false);

        setYSliderInvEnabled(false);
        boxYInv.setSelected(false);
        colorButtonYInv.setEnabled(false);
        removeClipSliceYInv();
        boundingCheckYInv.setSelected(false);

        setZSliderInvEnabled(false);
        boxZInv.setSelected(false);
        colorButtonZInv.setEnabled(false);
        removeClipSliceZInv();
        boundingCheckZInv.setSelected(false);
    }

    /**
     * Disable the arbitrary clipping.
     */
    public void disableClipA() {
        setASliderEnabled(false);
        boxA.setSelected(false);
        removeClipSliceA();
        boundingCheckA.setSelected(false);
        colorButtonA.setEnabled(false);
    }

    /**
     * Disable clipping planes when dialog window closed.
     */
    public void disableClipPlanes() {

        if (boxX.isSelected() || boundingCheckX.isSelected()) {
            setXSliderEnabled(false);
            boxX.setSelected(false);
            removeClipSliceX();
            boundingCheckX.setSelected(false);
            initClipSliceX();
            colorButtonX.setEnabled(false);
        }

        if (boxY.isSelected() || boundingCheckY.isSelected()) {
            setYSliderEnabled(false);
            boxY.setSelected(false);
            colorButtonY.setEnabled(false);
            removeClipSliceY();
            boundingCheckY.setSelected(false);
            initClipSliceY();
        }

        if (boxZ.isSelected() || boundingCheckZ.isSelected()) {
            setZSliderEnabled(false);
            boxZ.setSelected(false);
            colorButtonZ.setEnabled(false);
            removeClipSliceZ();
            boundingCheckZ.setSelected(false);
            initClipSliceZ();
        }

        if (boxXInv.isSelected() || boundingCheckXInv.isSelected()) {
            setXSliderInvEnabled(false);
            boxXInv.setSelected(false);
            colorButtonXInv.setEnabled(false);
            removeClipSliceXInv();
            boundingCheckXInv.setSelected(false);
            initClipSliceXInv();
        }

        if (boxYInv.isSelected() || boundingCheckYInv.isSelected()) {
            setYSliderInvEnabled(false);
            boxYInv.setSelected(false);
            colorButtonYInv.setEnabled(false);
            removeClipSliceYInv();
            boundingCheckYInv.setSelected(false);
            initClipSliceYInv();
        }

        if (boxZInv.isSelected() || boundingCheckZInv.isSelected()) {
            setZSliderInvEnabled(false);
            boxZInv.setSelected(false);
            colorButtonZInv.setEnabled(false);
            removeClipSliceZInv();
            boundingCheckZInv.setSelected(false);
            initClipSliceZInv();
        }
    }

    /**
     * Disable arbitrary clipping planes.
     */
    public void disableClipPlanesArbi() {

        if (boxA.isSelected() || boundingCheckA.isSelected()) {
            setASliderEnabled(false);
            boxA.setSelected(false);
            removeClipSliceA();
            boundingCheckA.setSelected(false);
            initClipSliceA();
            colorButtonA.setEnabled(false);
        }
    }

    /**
     * Disable the eye clipping plane.
     */
    public void disableStaticClipping() {

        if ((mcStatic != null) && mcStaticBG.isLive()) {
            mcStaticBG.detach();
            mcStatic.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        }

        setStaticSliderEnabled(false);
        colorButtonStatic.setEnabled(false);
        removeClipSliceStatic();
        boundingCheckStatic.setSelected(false);
        initClipSliceStatic();
        boxStatic.setSelected(false);
    }

    /**
     * Disable the eye inverse clipping plane.
     */
    public void disableStaticInvClipping() {

        if ((mcStatic != null) && mcStaticBG.isLive()) {
            mcStaticBG.detach();
            mcStatic.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        }

        setStaticInvSliderEnabled(false);
        colorButtonStaticInv.setEnabled(false);
        removeClipSliceStaticInv();
        boundingCheckStaticInv.setSelected(false);
        initClipSliceStaticInv();
        boxStaticInv.setSelected(false);
    }

    /**
     * Display the abitrary clipping plane corner points.
     */
    public void displayAClipPlanePts() {

        Transform3D t2, result;

        for (int i = 0; i < 12; i++) {
            Transform3D t = new Transform3D();

            t.setTranslation(new javax.vecmath.Vector3f(-999, -999, -999));
            m_akLightTransformGroup[i].setTransform(t);
        }

        t2 = new Transform3D();

        result = new Transform3D();
        mcArbiTG.getTransform(t2);
        result.set(t2);

        AclipPlanePts[0] = new javax.vecmath.Vector4f((float) (-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox)),
                                        maxBox * radicalRatio, maxBox * radicalRatio, 0);
        AclipPlanePts[1] = new javax.vecmath.Vector4f((float) (-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox)),
                                        -maxBox * radicalRatio, maxBox * radicalRatio, 0);
        AclipPlanePts[2] = new javax.vecmath.Vector4f((float) (-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox)),
                                        -maxBox * radicalRatio, -maxBox * radicalRatio, 0);
        AclipPlanePts[3] = new javax.vecmath.Vector4f((float) (-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox)),
                                        maxBox * radicalRatio, -maxBox * radicalRatio, 0);

        for (int i = 0; i < 4; i++) {
            result.transform(AclipPlanePts[i]);
            drawSphere(i, AclipPlanePts[i].x, AclipPlanePts[i].y, AclipPlanePts[i].z);
        }
        // getAClipPlanePts();
    }

    /**
     * Display the static(eye) clipping plane corner points.
     */
    public void displaySClipPlanePts() {

        if ((clipSliceStatic == null) || !boundingCheckStatic.isSelected()) {
            return;
        }

        Point3f[] vertex = clipSliceStatic.verts;

        AclipPlanePts[0] = new javax.vecmath.Vector4f(vertex[0].x, vertex[0].y, vertex[0].z, 0);
        AclipPlanePts[1] = new javax.vecmath.Vector4f(vertex[3].x, vertex[3].y, vertex[3].z, 0);
        AclipPlanePts[2] = new javax.vecmath.Vector4f(vertex[2].x, vertex[2].y, vertex[2].z, 0);
        AclipPlanePts[3] = new javax.vecmath.Vector4f(vertex[1].x, vertex[1].y, vertex[1].z, 0);

        for (int i = 0; i < 4; i++) {
            transform(AclipPlanePts[i]);
            drawSphere(i, AclipPlanePts[i].x, AclipPlanePts[i].y, AclipPlanePts[i].z);
        }

    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        clipSliderX = null;
        clipSliderY = null;
        clipSliderZ = null;
        clipSliderStatic = null;
        clipSliderStaticInv = null;
        sliderXInv = null;
        sliderYInv = null;
        sliderZInv = null;
        sliderA = null;
        boxX = null;
        boxY = null;
        boxZ = null;
        boxStatic = null;
        boxStaticInv = null;
        boxXInv = null;
        boxYInv = null;
        boxZInv = null;
        boxA = null;
        labelX = null;
        labelY = null;
        labelZ = null;
        labelStatic = null;
        labelStaticInv = null;
        labelXInv = null;
        labelYInv = null;
        labelZInv = null;
        labelA = null;
        labelXStart = null;
        labelXMid = null;
        labelXEnd = null;
        labelYStart = null;
        labelYMid = null;
        labelYEnd = null;
        labelZStart = null;
        labelZMid = null;
        labelZEnd = null;
        labelXStartInv = null;
        labelXMidInv = null;
        labelXEndInv = null;
        labelYStartInv = null;
        labelYMidInv = null;
        labelYEndInv = null;
        labelZStartInv = null;
        labelZMidInv = null;
        labelZEndInv = null;
        labelAStart = null;
        labelAMid = null;
        labelAEnd = null;
        labelStaticStart = null;
        labelStaticMid = null;
        labelStaticEnd = null;
        labelStaticInvStart = null;
        labelStaticInvMid = null;
        labelStaticInvEnd = null;
        textX = null;
        textY = null;
        textZ = null;
        textStatic = null;
        textStaticInv = null;
        textXInv = null;
        textYInv = null;
        textZInv = null;
        textA = null;
        colorButtonX = null;
        colorButtonXInv = null;
        colorButtonY = null;
        colorButtonYInv = null;
        colorButtonZ = null;
        colorButtonZInv = null;
        colorButtonStatic = null;
        colorButtonStaticInv = null;
        colorButtonA = null;
        colorChooser = null;
        boundingCheckX = null;
        boundingCheckY = null;
        boundingCheckZ = null;
        boundingCheckXInv = null;
        boundingCheckYInv = null;
        boundingCheckZInv = null;
        boundingCheckA = null;
        boundingCheckStatic = null;
        boundingCheckStaticInv = null;
        clipEvents = null;
        objClipSlices_BG = null;
        objClipSliceX_BG = null;
        objClipSliceY_BG = null;
        objClipSliceZ_BG = null;
        objClipSliceA_BG = null;
        clipSliceA_BG = null;
        objClipSliceStatic_BG = null;
        objClipSliceStaticInv_BG = null;
        objClipSliceXInv_BG = null;
        objClipSliceYInv_BG = null;
        objClipSliceZInv_BG = null;
        clipSliceA = null;
        clipSliceX = null;
        clipSliceY = null;
        clipSliceZ = null;
        clipSliceXInv = null;
        clipSliceYInv = null;
        clipSliceZInv = null;
        clipSliceStatic = null;
        clipSliceStaticInv = null;
        enables = null;
        enablesStatic = null;
        enablesArbi = null;
        mcBG = null;
        mcArbiBG = null;
        mcStaticBG = null;
        mcExtendBG = null;
        mcArbiExtendBG = null;
        mc = null;
        mcArbi = null;
        mcStatic = null;
        eqnX = null;
        eqnXInv = null;
        eqnY = null;
        eqnYInv = null;
        eqnZ = null;
        eqnZInv = null;
        eqnA = null;
        eqnPlanes = null;
        eqnPlanesArbi = null;
        eqnPlanesStatic = null;
        arbiTG = null;
        arbiMouseRotateBehavior = null;
        arbiTrans3d = null;
        mcArbiTrans3D = null;
        mcArbiTG = null;
        rotationAxis = null;
        rotationEvent = null;
        tabbedPane = null;
        panelX = null;
        panelXInv = null;
        panelY = null;
        panelYInv = null;
        panelZ = null;
        panelZInv = null;
        panelA = null;
        panelS = null;
        panelSInv = null;
        m_aiImageA_backup = null;
        m_aiImageB_backup = null;

    }

    /**
     * Enable arbitrary clipping plane behavior.
     *
     * @param  flag  <code>true</code> enable arbitrary clipping mouse behavior, disable the scene graph mouse behavior;
     *               <code>false</code> disable arbitrary clipping mouse behavior, enable the scene graph mouse
     *               behavior.
     */
    public void enableClipArbiBehavior(boolean flag) {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        // arbiMouseRotateBehavior.setEnable( flag );
        if (flag) {

            if (!behaviorBG.isLive()) {
                arbiTG.addChild(behaviorBG);
            }

            arbitrary_SG.setWhichChild(1);

        } else {

            if (behaviorBG.isLive()) {
                behaviorBG.detach();
            }

            arbitrary_SG.setWhichChild(0);
        }
    }

    /**
     * Enable the eye clipping plane.
     */
    public void enableStaticClipping() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (!mcStaticBG.isLive()) {
            renderBase.getBranchGroup().addChild(mcStaticBG);
            mcStatic.addScope(((SurfaceRender) renderBase).getVolRenderBG());
        }

        mcArbi.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        mc.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        mcArbiExtendBG.detach();
        mcExtendBG.detach();
    }

    /**
     * Enable the eye inverse clipping plane.
     */
    public void enableStaticInvClipping() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (!mcStaticBG.isLive()) {
            renderBase.getBranchGroup().addChild(mcStaticBG);
            mcStatic.addScope(((SurfaceRender) renderBase).getVolRenderBG());
        }

        mcArbi.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        mc.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
        mcArbiExtendBG.detach();
        mcExtendBG.detach();
    }

    /**
     * Transform Euler angle to quaterion. Not used for now.
     *
     * @param  v  Vector3d Euler angles
     * @param  q  Quat4d quaternion
     */
    public final void EulerToQuat(Vector3d v, Quat4d q) {

        // Assuming the angles are in radians.
        // v.x = roll;
        // v.y = pitch;
        // v.z = yaw;
        double roll, pitch, yaw;

        roll = v.x;
        pitch = v.y;
        yaw = v.z;

        double w, x, y, z;
        double c1 = Math.cos(pitch / 2);
        double s1 = Math.sin(pitch / 2);
        double c2 = Math.cos(yaw / 2);
        double s2 = Math.sin(yaw / 2);
        double c3 = Math.cos(roll / 2);
        double s3 = Math.sin(roll / 2);
        double c1c2 = c1 * c2;
        double s1s2 = s1 * s2;

        w = (c1c2 * c3) - (s1s2 * s3);
        x = (c1c2 * s3) + (s1s2 * c3);
        y = (s1 * c2 * c3) + (c1 * s2 * s3);
        z = (c1 * s2 * c3) - (s1 * c2 * s3);
        q.w = w;
        q.x = x;
        q.y = y;
        q.z = z;
    }

    /**
     * Get the arbitrary clipping plane four corners points.
     *
     * @return  Vector3f[]
     */
    public WildMagic.LibFoundation.Mathematics.Vector3f[] getAClipPlanePts() {
    	WildMagic.LibFoundation.Mathematics.Vector3f[] pts = new WildMagic.LibFoundation.Mathematics.Vector3f[4];
        float m_fX0;
        float m_fY0;
        float m_fZ0;
        float m_fX1;
        float m_fY1;
        float m_fZ1;
        int j;
        WildMagic.LibFoundation.Mathematics.Vector3f[] cornerPts = new WildMagic.LibFoundation.Mathematics.Vector3f[4];

        for (int i = 0; i < 4; i++) {
            cornerPts[i] = new WildMagic.LibFoundation.Mathematics.Vector3f();
        }

        m_fX0 = -xBox;
        m_fY0 = -yBox;
        m_fX1 = xBox;
        m_fY1 = yBox;

        m_fZ0 = -zBox;
        m_fZ1 = zBox;

        if (zBox > maxBox) {
            m_fZ0 = -1f;
            m_fZ1 = 1f;
        }

        for (j = 0; j < 4; j++) {

            // transform from volume space to image space
            // using the bounding box approach
            cornerPts[j].X = ((AclipPlanePts[j].x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
            cornerPts[j].Y = ((AclipPlanePts[j].y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
            cornerPts[j].Z = ((AclipPlanePts[j].z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

            // flip the z and y coordinate
            cornerPts[j].Z = zDim - 1 - cornerPts[j].Z;
            cornerPts[j].Y = yDim - 1 - cornerPts[j].Y;

            pts[j] = new WildMagic.LibFoundation.Mathematics.Vector3f();
        }

        // go from counter-clockwise to upleft, lowleft, lowright, upright
        pts[0].X = cornerPts[0].X;
        pts[0].Y = cornerPts[0].Y;
        pts[0].Z = cornerPts[0].Z;
        pts[1].X = cornerPts[3].X;
        pts[1].Y = cornerPts[3].Y;
        pts[1].Z = cornerPts[3].Z;
        pts[2].X = cornerPts[1].X;
        pts[2].Y = cornerPts[1].Y;
        pts[2].Z = cornerPts[1].Z;
        pts[3].X = cornerPts[2].X;
        pts[3].Y = cornerPts[2].Y;
        pts[3].Z = cornerPts[2].Z;

        return pts;
    }

    /**
     * Return check box arbitrary selection value.
     *
     * @return  boxA.isSelected() box arbitrary slection value.
     */
    public boolean getAVisible() {
        return boxA.isSelected();
    }

    /**
     * Get rotation axis alpha value.
     *
     * @return  aixsAngle alpha value.
     */
    public float getAxisAngle() {
        return axisAngle;
    }

    /**
     * Get rotation axis X value.
     *
     * @return  aixsX x value.
     */
    public float getAxisX() {
        return axisX;
    }

    /**
     * Get rotation axis Y value.
     *
     * @return  aixsY y value.
     */
    public float getAxisY() {
        return axisY;
    }

    /**
     * Get rotation axis Z value.
     *
     * @return  aixsZ z value.
     */
    public float getAxisZ() {
        return axisZ;
    }

    /**
     * Get the location of the negative X slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundXInv() {
        float value = (xSliceInv - 1) / (float) (xDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the location of the positive X slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundXPos() {
        float value = (xSlice - 1) / (float) (xDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the location of the negative Y slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundYInv() {
        float value = (ySliceInv - 1) / (float) (yDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the location of the positive Y slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundYPos() {
        float value = (ySlice - 1) / (float) (yDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the location of the negative Z slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundZInv() {
        float value = (zSliceInv - 1) / (float) (zDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the location of the positive Z slice bound.
     *
     * @return  float Value in normalized range of [0,1].
     */
    public float getBoundZPos() {
        float value = (zSlice - 1) / (float) (zDim - 1);

        return Math.max(0.0f, Math.min(value, 1.0f));
    }

    /**
     * Get the main control panel.
     *
     * @return  mainPanel the whole control panel.
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Get arbitray slider value.
     *
     * @return  Arbitrary slider value.
     */
    public int getSliceA() {
        return aSlice;
    }

    /**
     * Get x slider value.
     *
     * @return  X clipping slider value.
     */
    public int getSliceX() {
        return xSlice;
    }

    /**
     * Get x negative slider value.
     *
     * @return  Get y clipping slider value
     */
    public int getSliceXInv() {
        return xSliceInv;
    }

    /**
     * Get y slider value.
     *
     * @return  Get y clipping slider value.
     */
    public int getSliceY() {
        return ySlice;
    }

    /**
     * Get y negative slider value.
     *
     * @return  Get y inverse clipping slider value.
     */
    public int getSliceYInv() {
        return ySliceInv;
    }

    /**
     * Get z slider value.
     *
     * @return  Get Z clippig slider value.
     */
    public int getSliceZ() {
        return zSlice;
    }

    /**
     * Get z negative slider value.
     *
     * @return  get z inverse clipping slider value.
     */
    public int getSliceZInv() {
        return zSliceInv;
    }

    /**
     * Get arbitrary slider.
     *
     * @return  get arbitrary clipping slider value.
     */
    public JSlider getSliderA() {
        return sliderA;
    }

    /**
     * Get x slider.
     *
     * @return  get x clipping slider value.
     */
    public JSlider getSliderX() {
        return clipSliderX;
    }

    /**
     * Get x negative slider.
     *
     * @return  get X inverse clipping slider value.
     */
    public JSlider getSliderXInv() {
        return sliderXInv;
    }

    /**
     * Get y slider.
     *
     * @return  get Y clipping slider value.
     */
    public JSlider getSliderY() {
        return clipSliderY;
    }

    /**
     * Get y negative slider.
     *
     * @return  get y inverse clipping slider value.
     */
    public JSlider getSliderYInv() {
        return sliderYInv;
    }

    /**
     * Get z slider.
     *
     * @return  get z clipping slider value.
     */
    public JSlider getSliderZ() {
        return clipSliderZ;
    }

    /**
     * Get z negative slider.
     *
     * @return  get z inverse clipping slider value.
     */
    public JSlider getSliderZInv() {
        return sliderZInv;
    }

    /**
     * Return check box static inverse selection value.
     *
     * @return  boxZInv.isSelected() box z negative slection value.
     */
    public boolean getStaticInvVisible() {
        return boxStaticInv.isSelected();
    }

    /**
     * Return check box static selection value.
     *
     * @return  boxZInv.isSelected() box z negative slection value.
     */
    public boolean getStaticVisible() {
        return boxStatic.isSelected();
    }

    /**
     * Return check box X selection value.
     *
     * @return  boxX.isSelected() box X selection value.
     */
    public boolean getXVisible() {
        return boxX.isSelected();
    }

    /**
     * Return check box X negative selection value.
     *
     * @return  boxXInv.isSelected() box x negative slection value.
     */
    public boolean getXVisibleInv() {
        return boxXInv.isSelected();
    }

    /**
     * Return check box Y selection value.
     *
     * @return  boxY.isSelected() box Y selection value.
     */
    public boolean getYVisible() {
        return boxY.isSelected();
    }

    /**
     * Return check box Y negative selection value.
     *
     * @return  boxYInv.isSelected() box y negative slection value.
     */
    public boolean getYVisibleInv() {
        return boxYInv.isSelected();
    }

    /**
     * Return check box Z selection value.
     *
     * @return  boxZ.isSelected() box z selection value.
     */
    public boolean getZVisible() {
        return boxZ.isSelected();
    }

    /**
     * Return check box Z negative selection value.
     *
     * @return  boxZInv.isSelected() box z negative slection value.
     */
    public boolean getZVisibleInv() {
        return boxZInv.isSelected();
    }

    /**
     * Hides the slice frame on arbitrary clipping plane slice.
     */
    public void hideClipSliceA() {

        if ((objClipSliceA_BG != null) && objClipSliceA_BG.isLive()) {
            objClipSliceA_BG.detach();
        }

        if (m_kLightRoot.isLive()) {
            m_kLightRoot.detach();
        }
    }

    /**
     * Hides the static clipping plane box frame.
     */
    public void hideClipSliceStatic() {

        if ((objClipSliceStatic_BG != null) && objClipSliceStatic_BG.isLive()) {
            objClipSliceStatic_BG.detach();
        }

        if (m_kLightRoot.isLive()) {
            m_kLightRoot.detach();
        }

    }

    /**
     * Hides the static inverset clipping plane box frame.
     */
    public void hideClipSliceStaticInv() {

        if ((objClipSliceStaticInv_BG != null) && objClipSliceStaticInv_BG.isLive()) {
            objClipSliceStaticInv_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice x.
     */
    public void hideClipSliceX() {

        if ((objClipSliceX_BG != null) && objClipSliceX_BG.isLive()) {
            objClipSliceX_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice -x.
     */
    public void hideClipSliceXInv() {

        if ((objClipSliceXInv_BG != null) && objClipSliceXInv_BG.isLive()) {
            objClipSliceXInv_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice y.
     */
    public void hideClipSliceY() {

        if ((objClipSliceY_BG != null) && objClipSliceY_BG.isLive()) {
            objClipSliceY_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice -y.
     */
    public void hideClipSliceYInv() {

        if ((objClipSliceYInv_BG != null) && objClipSliceYInv_BG.isLive()) {
            objClipSliceYInv_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice z.
     */
    public void hideClipSliceZ() {

        if ((objClipSliceZ_BG != null) && objClipSliceZ_BG.isLive()) {
            objClipSliceZ_BG.detach();
        }
    }

    /**
     * Hides the slice frame on clipping plane slice -z.
     */
    public void hideClipSliceZInv() {

        if ((objClipSliceZInv_BG != null) && objClipSliceZInv_BG.isLive()) {
            objClipSliceZInv_BG.detach();
        }
    }

    /**
     * Initializes GUI components.
     */
    public void init() {

        // setTitle("Clipping Planes");
        Box contentBox = new Box(BoxLayout.Y_AXIS);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        buildPanelX();
        buildPanelXInv();
        buildPanelY();
        buildPanelYInv();
        buildPanelZ();
        buildPanelZInv();
        buildPanelA();
        buildPanelS();
        buildPanelSInv();

        tabbedPane.setSelectedIndex(0);
        tabbedPane.addChangeListener(this);
        contentBox.add(tabbedPane);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }

    /**
     * Move the arbitrary clipping bounding frame to initial position.
     */
    public void initClipSliceA() {
        if ( mcArbi == null )
        {
            return;
        }

        if (rotationAxis != null) {
            mcArbiTrans3D.set(rotationAxis);
            arbiTrans3d.set(rotationAxis);
        }

        mcArbiTG.setTransform(mcArbiTrans3D);
        arbiTG.setTransform(arbiTrans3d);

        eqnA = new Vector4d(1.0, 0.0, 0.0, (maxBox - (2 * ((float) aSlice / (maxDim - 1)) * maxBox) - 0.001));
        updateClipPlanesEqn();
        clipSliceA.setSlices(1, maxBox, maxBox, ViewJComponentBoxSlice.A_CLIPSLICE);

        mcArbi.setEnable(0, false);

        Shape3D shape = new Shape3D(clipSliceA, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);
		PickTool.setCapabilities(shape, PickTool.INTERSECT_FULL);
        
        try {
            clipSliceA_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the static clipping bounding frame to initial position.
     */
    public void initClipSliceStatic() {
        if (mcStatic == null)
        {
            return;
        }

        clipSliceStatic.setSlices(xBox * 0.5f, yBox * 0.5f, zBox, ViewJComponentBoxSlice.S_CLIPSLICE);
        mcStatic.setEnable(5, false);

        Shape3D shape = new Shape3D(clipSliceStatic, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceStatic_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the static inverse clipping bounding frame to initial position.
     */
    public void initClipSliceStaticInv() {
        if (mcStatic == null)
        {
            return;
        }

        clipSliceStaticInv.setSlices(xBox * 0.5f, yBox * 0.5f, -zBox, ViewJComponentBoxSlice.S_CLIPSLICE_NEG);
        mcStatic.setEnable(4, false);

        Shape3D shape = new Shape3D(clipSliceStaticInv, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceStaticInv_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the X clipping bounding frame to initial position.
     */
    public void initClipSliceX() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        clipSliceX.setSlices(1, yBox, zBox, ViewJComponentBoxSlice.X_CLIPSLICE);
        mc.setEnable(0, false);

        Shape3D shape = new Shape3D(clipSliceX, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceX_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the X Invative clipping bounding frame to initial position.
     */
    public void initClipSliceXInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        clipSliceXInv.setSlices(-1, yBox, zBox, ViewJComponentBoxSlice.X_CLIPSLICE_NEG);
        mc.setEnable(1, false);

        Shape3D shape = new Shape3D(clipSliceXInv, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceXInv_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the Y clipping bounding frame to initial position.
     */
    public void initClipSliceY() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }
        clipSliceY.setSlices(xBox, -1, zBox, ViewJComponentBoxSlice.Y_CLIPSLICE);
        mc.setEnable(2, false);

        Shape3D shape = new Shape3D(clipSliceY, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceY_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the Y Nagative clipping bounding frame to initial position.
     */
    public void initClipSliceYInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        clipSliceYInv.setSlices(xBox, 1, zBox, ViewJComponentBoxSlice.Y_CLIPSLICE_NEG);
        mc.setEnable(3, false);

        Shape3D shape = new Shape3D(clipSliceYInv, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceYInv_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the Z clipping bounding frame to initial position.
     */
    public void initClipSliceZ() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        clipSliceZ.setSlices(xBox, yBox, -zBox, ViewJComponentBoxSlice.Z_CLIPSLICE);
        mc.setEnable(4, false);

        Shape3D shape = new Shape3D(clipSliceZ, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceZ_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Move the Z Invative clipping bounding frame to initial position.
     */
    public void initClipSliceZInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        clipSliceZInv.setSlices(xBox, yBox, zBox, ViewJComponentBoxSlice.Z_CLIPSLICE_NEG);
        mc.setEnable(5, false);

        Shape3D shape = new Shape3D(clipSliceZInv, null);

        shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
        shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
        shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

        try {
            objClipSliceZInv_BG.addChild(shape);
        } catch (RestrictedAccessException error) { }
    }

    /**
     * Invokes all the 6 clipping when 6 clipping checkbox is checked.
     */
    public void invokeClippingPlanes() {
        disable6Planes();
        boundingCheckX.setSelected(true);
        boundingCheckY.setSelected(true);
        boundingCheckZ.setSelected(true);
        boundingCheckXInv.setSelected(true);
        boundingCheckYInv.setSelected(true);
        boundingCheckZInv.setSelected(true);
        boxX.setSelected(true);
        boxY.setSelected(true);
        boxZ.setSelected(true);
        boxXInv.setSelected(true);
        boxYInv.setSelected(true);
        boxZInv.setSelected(true);
        addClipSliceX();
        colorButtonX.setEnabled(true);
        addClipSliceXInv();
        colorButtonXInv.setEnabled(true);
        addClipSliceY();
        colorButtonY.setEnabled(true);
        addClipSliceYInv();
        colorButtonYInv.setEnabled(true);
        addClipSliceZ();
        colorButtonZ.setEnabled(true);
        addClipSliceZInv();
        colorButtonZInv.setEnabled(true);
        swapModelClipBG(true);
        setXSliderEnabled(true);
        updateClipSliceX();

        setYSliderEnabled(true);
        updateClipSliceY();

        setZSliderEnabled(true);
        updateClipSliceZ();

        setXSliderInvEnabled(true);
        updateClipSliceXInv();

        setYSliderInvEnabled(true);
        updateClipSliceYInv();

        setZSliderInvEnabled(true);
        updateClipSliceZInv();

        updateVolumeRenderClipPlane();
        disableClipPlanesArbi();
    }

    /**
     * Indicate whether the 6 clipping plane is active or not.
     *
     * @return  is6PlaneClipping <code>true</code> 6 clipping plane active, <code>false</code> arbitrary clippiing plane
     *          active.
     */
    public boolean is6PlaneClipping() {
        return is6PlaneClipping;
    }

    /**
     * Called by the JDialogSurface to find the current mouse event is from the arbitrary frame box moving or not.
     *
     * @param   pickeObject  Shape3D picked object from the JDialogSurface.
     *
     * @return  <code>true</code> arbitrary frame box moves, <code>false</code> not moves
     */
    public boolean isArbitraryClipping(Shape3D pickeObject) {
        isClipArbiPicked = false;

        if ((pickeObject == null) || !objClipSliceA_BG.isLive()) {
            return false;
        }

        if (clipSliceA_BG.getChild(0) == pickeObject) {
            isClipArbiPicked = true;
        } else {
            isClipArbiPicked = false;
        }

        return isClipArbiPicked;
    }

    /**
     * Return the flag for the arbitrary clipping plane frame being picked or not.
     *
     * @return  isClipArbiPicked <code>true</code> picked, <code>false</code> not picked.
     */
    public boolean isClipArbiPicked() {
        return isClipArbiPicked;
    }

    /**
     * Return whether the clipping planes tree branch being built the first time.
     *
     * @return  isFirstTimeBuildTree if <code>true</code> first time build the tree, <code>false</code> already built.
     */
    public boolean isFirstTimeBuildTree() {
        return isFirstTimeBuildTree;
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * If recording, adds this mouse event to the mouseEvents vector found at location <code>current</code>.
     *
     * @param  event  Original mouse event.
     */
    public void mouseDragged(MouseEvent event) {
        JPanelMouse myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();

        if (myMouseDialog.isPlaying() && !isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
            return;
        }

        if (myMouseDialog.isRecording() && isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {

            if (isTransformChanged) {
                rotationEvent.add(event, renderBase.getSceneState());
            }
        }
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseMoved(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {
        JPanelMouse myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();

        if (myMouseDialog.isPlaying()) {

            if (!isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
                isClipArbiPicked = false;
                enableClipArbiBehavior(false);

                return;
            } else if (isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
                isClipArbiPicked = true;
                enableClipArbiBehavior(true);
            }
        }

        if (myMouseDialog.isRecording() && !isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
            Transform3D t3D = new Transform3D();
            renderBase.getSceneRootTG().getTransform(t3D);
            double[] mat = new double[16];
            t3D.get(mat);
            clipEvents = new MouseEventVector("clipSlider" + clipCount, mat, myMouseDialog.first,
                                              renderBase.getSceneState(), ((SurfaceRender) renderBase).getMouseMode());
            recordEventName = true;
        } else if (myMouseDialog.isRecording() && isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
            Transform3D t3D = new Transform3D();
            renderBase.getSceneRootTG().getTransform(t3D);
            double[] mat = new double[16];
            t3D.get(mat);
            rotationEvent = new MouseEventVector("ArbitraryRotation" + rotationCount, mat, myMouseDialog.first,
                                                 renderBase.getSceneState(),
                                                 ((SurfaceRender) renderBase).getMouseMode());
            recordEventName = true;
            rotationEvent.add(event, renderBase.getSceneState());
        }
    }

    /**
     * Used in MouseRecorder to stop one series of slide moves.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {
        Object source = event.getSource();
        JPanelMouse myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();

        if (myMouseDialog.isPlaying() && !isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {
            return;
        }

        if (myMouseDialog.isRecording() && !isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {

            if (isStateChangedEvent) {
                myMouseDialog.events.add(clipEvents);
                clipCount++;
            }
        }

        if (myMouseDialog.isRecording() && isClipArbiPicked && ((SurfaceRender) renderBase).getDisplayMode3D()) {

            if (isTransformChanged) {

                // System.out.println("mouseReleased clipDialog record");
                rotationEvent.add(event, renderBase.getSceneState());
                myMouseDialog.events.add(rotationEvent);
                rotationCount++;
            }
        }

        isStateChangedEvent = false;
        isTransformChanged = false;
        isClipArbiPicked = false;

        /*
         * if ( arbiMouseRotateBehavior != null ) { enableClipArbiBehavior(false); }
         */
    }

    /**
     * Transform a quaternion to euler. This method get the Euler angles from the volume rotation.
     *
     * @param  q1  Quat4d quaternion.
     * @param  v   Vector3d euler
     */
    public void QuatToEuler(Quat4d q1, Vector3d v) {
        double pitch, yaw, roll;
        double test = (q1.x * q1.y) + (q1.z * q1.w);

        if (test > 0.499) { // singularity at north pole
            pitch = 2 * Math.atan2(q1.x, q1.w);
            yaw = Math.PI / 2;
            roll = 0;

            return;
        }

        if (test < -0.499) { // singularity at south pole
            pitch = -2 * Math.atan2(q1.x, q1.w);
            yaw = -Math.PI / 2;
            roll = 0;

            return;
        }

        double sqx = q1.x * q1.x;
        double sqy = q1.y * q1.y;
        double sqz = q1.z * q1.z;

        pitch = Math.atan2((2 * q1.y * q1.w) - (2 * q1.x * q1.z), 1 - (2 * sqy) - (2 * sqz));
        yaw = Math.asin(2 * test);
        roll = Math.atan2((2 * q1.x * q1.w) - (2 * q1.y * q1.z), 1 - (2 * sqx) - (2 * sqz));
        v.x = roll;
        v.y = pitch;
        v.z = yaw;
    }

    /**
     * Removes the clip slices.
     */
    public void removeClipSlice() {
        removeClipSliceA();
        removeClipSliceX();
        removeClipSliceY();
        removeClipSliceZ();
        removeClipSliceXInv();
        removeClipSliceYInv();
        removeClipSliceZInv();
    }

    /**
     * Detaches the slice frame on arbitrary clipping plane slice.
     */
    public void removeClipSliceA() {
        if ( mcArbi == null )
        {
            return;
        }

        if (objClipSliceA_BG.isLive()) {
            objClipSliceA_BG.detach();
            mcArbi.setEnable(0, false);
        }
    }

    /**
     * remove the static clipping plane box frame.
     */
    public void removeClipSliceStatic() {
        if (mcStatic == null)
        {
            return;
        }

        if (objClipSliceStatic_BG.isLive()) {
            objClipSliceStatic_BG.detach();
        }

        mcStatic.setEnable(5, false);
    }

    /**
     * remove the static inverse clipping plane box frame.
     */
    public void removeClipSliceStaticInv() {
        if (mcStatic == null)
        {
            return;
        }

        if (objClipSliceStaticInv_BG.isLive()) {
            objClipSliceStaticInv_BG.detach();
        }

        mcStatic.setEnable(4, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice x.
     */
    public void removeClipSliceX() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceX_BG.isLive()) {
            objClipSliceX_BG.detach();
        }

        mc.setEnable(0, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice -x.
     */
    public void removeClipSliceXInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceXInv_BG.isLive()) {
            objClipSliceXInv_BG.detach();
        }

        mc.setEnable(1, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice y.
     */
    public void removeClipSliceY() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceY_BG.isLive()) {
            objClipSliceY_BG.detach();
        }

        mc.setEnable(2, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice -y.
     */
    public void removeClipSliceYInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceYInv_BG.isLive()) {
            objClipSliceYInv_BG.detach();
        }

        mc.setEnable(3, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice z.
     */
    public void removeClipSliceZ() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceZ_BG.isLive()) {
            objClipSliceZ_BG.detach();
        }

        mc.setEnable(4, false);
    }

    /**
     * Detaches the slice frame on clipping plane slice -z.
     */
    public void removeClipSliceZInv() {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (objClipSliceZInv_BG.isLive()) {
            objClipSliceZInv_BG.detach();
        }

        mc.setEnable(5, false);
    }

    /**
     * Remove the branch group passed in.
     *
     * @param  root  BranchGroup node to be removed.
     */
    public void removeFromModelClip(BranchGroup root) {
        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        int index = -1;

        index = mc.indexOfScope(root);

        if (index != -1) {
            mc.removeScope(index);
        }

        index = mcArbi.indexOfScope(root);

        if (index != -1) {
            mcArbi.removeScope(index);
        }

        index = mcStatic.indexOfScope(root);

        if (index != -1) {
            mcStatic.removeScope(index);
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width
     * @param  frameHeight  parent frame height.
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }

    /**
     * Save the cropped image.
     */
    public void saveCropImage() {
        FileWriteOptions options;
        options = new FileWriteOptions(true);

        int filterType = -1;
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ModelImage img = null;
        ViewImageFileFilter vFilter = null;
        ModelImage kImageA = ((SurfaceRender) renderBase).getImageA();
        ModelImage kImageB = ((SurfaceRender) renderBase).getImageB();

        int i;

        if (kImageA != null) {
            img = kImageA;
        } else if (kImageB != null) {
            img = kImageB;
        }

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, true);

                    
                        JFileChooser chooser = fileChooser.getFileChooser();

                        // chooser.setName("Save image as");
                        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
                        } else {
                            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                        }

                        if (filterType >= 0) {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(filterType));
                        } else {
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                        }

                        int returnVal = chooser.showSaveDialog(this);

                        if (returnVal == JFileChooser.APPROVE_OPTION) {
                            fileName = chooser.getSelectedFile().getName();

                            if (filterType >= 0) {
                                i = fileName.lastIndexOf('.');

                                if ((i > 0) && (i < (fileName.length() - 1))) {
                                    extension = fileName.substring(i + 1).toLowerCase();
                                    vFilter = new ViewImageFileFilter(filterType);

                                    if (!vFilter.accept(extension)) {
                                        MipavUtil.displayError("Extension does not match filter type");

                                        return;
                                    }
                                } // if ( i > 0 && i < fileName.length() - 1 )
                                else if (i < 0) {

                                    switch (filterType) {

                                        case ViewImageFileFilter.AVI:
                                            fileName = fileName + ".avi";
                                            break;

                                        case ViewImageFileFilter.VOI:
                                            fileName = fileName + ".voi";
                                            break;

                                        case ViewImageFileFilter.FUNCT:
                                            fileName = fileName + ".fun";
                                            break;

                                        case ViewImageFileFilter.LUT:
                                            fileName = fileName + ".lut";
                                            break;

                                        case ViewImageFileFilter.PLOT:
                                            fileName = fileName + ".plt";
                                            break;

                                        case ViewImageFileFilter.CLASS:
                                            fileName = fileName + ".class";
                                            break;

                                        case ViewImageFileFilter.SCRIPT:
                                            fileName = fileName + ".sct";
                                            break;

                                        case ViewImageFileFilter.SURFACE:
                                            fileName = fileName + ".sur";
                                            break;

                                        case ViewImageFileFilter.FREESURFER:
                                            fileName = fileName + ".asc";
                                            break;
                                    }
                                } // else if (i < 0)
                            } // if (filterType >= 0)

                            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                            ViewUserInterface.getReference().setDefaultDirectory(directory);
                        } else {
                            return;
                        }
                    
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return;
                }
            }

        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

        /*
         * I'm not sure why this wasn't done before.... if we do a save-as we should also update the name of the file
         */
        // if (options.isSaveAs()) {
        // img.setImageName(fileName.substring(0, fileName.length()-4));
        // }

        options.setFileName(fileName);
        options.setFileDirectory(directory);

        if (!options.isSaveAs()) {

            if (img.getNDims() == 3) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
            } else if (img.getNDims() == 4) {
                options.setBeginSlice(0);
                options.setEndSlice(img.getExtents()[2] - 1);
                options.setBeginTime(0);
                options.setEndTime(img.getExtents()[3] - 1);
            }
        }

        if (fileName != null) {
            FileIO fileIO = new FileIO();
            fileIO.writeImage(img, options);
        }

        // if the SaveAllOnSave preference flag is set, then
        // save all the files associated with this image (VOIs, LUTs, etc.)
        if (Preferences.is(Preferences.PREF_SAVE_ALL_ON_SAVE)) {

            // Since the options may have changed the filename
            // and the directory --- get new fileName and directory
            // from options
            String fName = options.getFileName(); // if you use the name from img, then DICOM has funny names
            String dirName = img.getFileInfo(0).getFileDirectory();
            String filebase;
            int ind = fName.lastIndexOf(".");

            if (ind > 0) {
                filebase = fName.substring(0, fName.lastIndexOf("."));
            } else {
                filebase = new String(fName);
            }

            if (options.getFileType() == FileUtility.DICOM) {
                int newIndex = filebase.length();

                for (i = filebase.length() - 1; i >= 0; i--) {
                    char myChar = filebase.charAt(i);

                    if (Character.isDigit(myChar)) {
                        newIndex = i;
                    } else {
                        break;
                    } // as soon as something is NOT a digit, leave loop
                }

                if (newIndex > 0) {
                    filebase = filebase.substring(0, newIndex);
                }
            }

            // save any luts
            String lutName = new String(filebase + ".lut");
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);
            fileType = FileUtility.getFileType(fileName, directory, false);
        }

        // now, get rid of any numbers at the end of the name (these
        // are part of the dicom file name, but we only want the 'base'
        // part of the name
        String baseName = new String(fileName);

        if (fileType == FileUtility.DICOM) {
            int index = fileName.lastIndexOf(".");

            if (index > 0) {
                baseName = fileName.substring(0, index);
            }

            int newIndex = baseName.length();

            for (i = baseName.length() - 1; i >= 0; i--) {
                char myChar = baseName.charAt(i);

                if (Character.isDigit(myChar)) {
                    newIndex = i;
                } else {
                    break;
                } // as soon as something is NOT a digit, leave loop
            }

            if (newIndex > 0) {
                baseName = baseName.substring(0, newIndex);
            }

            fileName = new String(baseName + ".dcm");

            if (!directory.endsWith(baseName)) {
                directory = new String(directory + baseName + File.separator);
            }
        }

        for (i = 0; i < fileInfo.length; i++) {
            fileInfo[i].setFileDirectory(directory);

            if (fileType == FileUtility.DICOM) {
                fileInfo[i].setFileName(baseName + (i + 1) + ".dcm");
            } else {
                fileInfo[i].setFileName(fileName);
            }

            fileInfo[i].setFileSuffix(suffix);
            // fileInfo[i].setFileFormat (fileType);
        }
    }

    /**
     * setGUI setup the flag of the arbitary clipping frame box.
     *
     * @param  _picked  <code>true</code> picked, <code>false</code> not picked.
     */
    public void setArbiPlanePickable(boolean _picked) {
        isClipArbiPicked = _picked;
    }

    /**
     * Sets the arbitray clip slider and the labels. state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setASliderEnabled(boolean flag) {
        sliderA.setEnabled(flag);
        labelA.setEnabled(flag);
        labelAStart.setEnabled(flag);
        labelAMid.setEnabled(flag);
        labelAEnd.setEnabled(flag);
        if ( mcArbi != null )
        {
            mcArbi.setEnable(0, flag);
        }
    }

    /**
     * Set the slider arbitrary checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxA(boolean isSelected) {
        boxA.setSelected(isSelected);
    }

    /**
     * Set the slider Static checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxStatic(boolean isSelected) {
        boxStatic.setSelected(isSelected);
    }

    /**
     * Set the slider Static inverse checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxStaticInv(boolean isSelected) {
        boxStaticInv.setSelected(isSelected);
    }

    /**
     * Set the slider X checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxX(boolean isSelected) {
        boxX.setSelected(isSelected);
    }

    /**
     * Set the slider X inverse checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxXInv(boolean isSelected) {
        boxXInv.setSelected(isSelected);
    }

    /**
     * Set the slider Y checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxY(boolean isSelected) {
        boxY.setSelected(isSelected);
    }

    /**
     * Set the slider Y inverse checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxYInv(boolean isSelected) {
        boxYInv.setSelected(isSelected);
    }

    /**
     * Set the slider Z checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxZ(boolean isSelected) {
        boxZ.setSelected(isSelected);
    }

    /**
     * Set the slider Z inverse checkBox with the passed in value.
     *
     * @param  isSelected  if <code>true</code> selected, otherwise not selected.
     */
    public void setCheckBoxZInv(boolean isSelected) {
        boxZInv.setSelected(isSelected);
    }

    /**
     * Sets the color of the arbitrary clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceAColor(Color color) {
        clipSliceA.setColor(color);
    }

    /**
     * During mouse recorder diaplay, setup the rotatino axis for both arbiTG and mcArbiTG.
     *
     * @param  _axisX      rotation axis x value.
     * @param  _axisY      rotation axis y value.
     * @param  _axisZ      rotation axis z value.
     * @param  _axisAngle  rotation axis alpha value.
     */
    public void setClipSliceAwithRotate(float _axisX, float _axisY, float _axisZ, float _axisAngle) {
        rotationAxis = new AxisAngle4f();
        rotationAxis.x = _axisX;
        rotationAxis.y = _axisY;
        rotationAxis.z = _axisZ;
        rotationAxis.angle = _axisAngle;

        mcArbiTrans3D.set(rotationAxis);
        mcArbiTG.setTransform(mcArbiTrans3D);
        arbiTrans3d.set(rotationAxis);
        arbiTG.setTransform(arbiTrans3d);
    }

    /**
     * Sets the color of the static clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceSColor(Color color) {
        if ( clipSliceStatic != null )
        {
            clipSliceStatic.setColor(color);
        }
   }

    /**
     * Sets the color of the static inverse clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceSInvColor(Color color) {
        if ( clipSliceStaticInv != null )
        {
            clipSliceStaticInv.setColor(color);
        }
    }

    /**
     * Sets the color of the x clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceXColor(Color color) {
        if ( clipSliceX != null )
        {
            clipSliceX.setColor(color);
        }
    }

    /**
     * Sets the color of the -x clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceXInvColor(Color color) {
        if ( clipSliceXInv != null )
        {
            clipSliceXInv.setColor(color);
        }
    }

    /**
     * Sets the color of the y cliping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceYColor(Color color) {
        if ( clipSliceY != null )
        {
            clipSliceY.setColor(color);
        }
    }

    /**
     * Sets the color of the -y cliping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceYInvColor(Color color) {
        if ( clipSliceYInv != null )
        {
            clipSliceYInv.setColor(color);
        }
    }

    /**
     * Sets the color of the z clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceZColor(Color color) {
        if ( clipSliceZ != null )
        {
            clipSliceZ.setColor(color);
        }
    }

    /**
     * Sets the color of the -z clipping plane slice frame.
     *
     * @param  color  Color to set to.
     */
    public void setClipSliceZInvColor(Color color) {
        if ( clipSliceZInv != null )
        {
            clipSliceZInv.setColor(color);
        }
    }

    /**
     * Sets the static inverse slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setStaticInvSliderEnabled(boolean flag) {
        clipSliderStaticInv.setEnabled(flag);
        labelStaticInv.setEnabled(flag);
        labelStaticInvStart.setEnabled(flag);
        labelStaticInvMid.setEnabled(flag);
        labelStaticInvEnd.setEnabled(flag);
        if ( mcStatic != null )
        {
            mcStatic.setEnable(4, flag);
        }
    }

    /**
     * Sets the static slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setStaticSliderEnabled(boolean flag) {
        clipSliderStatic.setEnabled(flag);
        labelStatic.setEnabled(flag);
        labelStaticStart.setEnabled(flag);
        labelStaticMid.setEnabled(flag);
        labelStaticEnd.setEnabled(flag);
        if ( mcStatic != null )
        {
            mcStatic.setEnable(5, flag);
        }
    }

    /**
     * Sets the x slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setXSliderEnabled(boolean flag) {
        clipSliderX.setEnabled(flag);
        labelX.setEnabled(flag);
        labelXStart.setEnabled(flag);
        labelXMid.setEnabled(flag);
        labelXEnd.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(0, flag);
        }
    }

    /**
     * Sets the x slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setXSliderInvEnabled(boolean flag) {
        sliderXInv.setEnabled(flag);
        labelXInv.setEnabled(flag);
        labelXStartInv.setEnabled(flag);
        labelXMidInv.setEnabled(flag);
        labelXEndInv.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(1, flag);
        }
    }

    /**
     * Sets the y slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setYSliderEnabled(boolean flag) {
        clipSliderY.setEnabled(flag);
        labelY.setEnabled(flag);
        labelYStart.setEnabled(flag);
        labelYMid.setEnabled(flag);
        labelYEnd.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(2, flag);
        }
    }

    /**
     * Sets the y slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setYSliderInvEnabled(boolean flag) {
        sliderYInv.setEnabled(flag);
        labelYInv.setEnabled(flag);
        labelYStartInv.setEnabled(flag);
        labelYMidInv.setEnabled(flag);
        labelYEndInv.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(3, flag);
        }
    }

    /**
     * Sets the z slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setZSliderEnabled(boolean flag) {
        clipSliderZ.setEnabled(flag);
        labelZ.setEnabled(flag);
        labelZStart.setEnabled(flag);
        labelZMid.setEnabled(flag);
        labelZEnd.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(4, flag);
        }
    }

    /**
     * Sets the z slider and the labels beside and beneath it to the state given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setZSliderInvEnabled(boolean flag) {
        sliderZInv.setEnabled(flag);
        labelZInv.setEnabled(flag);
        labelZStartInv.setEnabled(flag);
        labelZMidInv.setEnabled(flag);
        labelZEndInv.setEnabled(flag);
        if ( mc != null )
        {
            mc.setEnable(5, flag);
        }
    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        JPanelMouse myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();

        isStateChangedEvent = true;

        if (source == clipSliderX) {
            xSlice = clipSliderX.getValue() - 1;

            if (xSlice < xSliceInv) {
                xSlice = xSliceInv;
                clipSliderX.setValue(xSlice);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderX" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderX" + clipCount);
                recordEventName = false;
            }

            textX.setText(String.valueOf(xSlice + 1));
            updateClipSliceX();
            
            if (!clipSliderX.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == clipSliderY) {
            ySlice = clipSliderY.getValue() - 1;

            if (ySlice < ySliceInv) {
                ySlice = ySliceInv;
                clipSliderY.setValue(ySlice);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderY" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderY" + clipCount);
                recordEventName = false;
            }

            textY.setText(String.valueOf(ySlice + 1));
            updateClipSliceY();

            if (!clipSliderY.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == clipSliderZ) {
            zSlice = clipSliderZ.getValue() - 1;

            if (zSlice < zSliceInv) {
                zSlice = zSliceInv;
                clipSliderZ.setValue(zSlice);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderZ" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderZ" + clipCount);
                recordEventName = false;
            }

            textZ.setText(String.valueOf(zSlice + 1));
            updateClipSliceZ();

            if (!clipSliderZ.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == sliderXInv) {
            xSliceInv = sliderXInv.getValue() - 1;

            if (xSliceInv > xSlice) {
                xSliceInv = xSlice;
                sliderXInv.setValue(xSliceInv);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderXInv" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderXInv" + clipCount);
                recordEventName = false;
            }

            textXInv.setText(String.valueOf(xSliceInv + 1));
            updateClipSliceXInv();

            if (!sliderXInv.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == sliderYInv) {
            ySliceInv = sliderYInv.getValue() - 1;

            if (ySliceInv > ySlice) {
                ySliceInv = ySlice;
                sliderYInv.setValue(ySliceInv);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderYInv" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderYInv" + clipCount);
                recordEventName = false;
            }

            textYInv.setText(String.valueOf(ySliceInv + 1));
            updateClipSliceYInv();

            if (!sliderYInv.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == sliderZInv) {
            zSliceInv = sliderZInv.getValue() - 1;

            if (zSliceInv > zSlice) {
                zSliceInv = zSlice;
                sliderZInv.setValue(zSliceInv);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderZInv" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderZInv" + clipCount);
                recordEventName = false;
            }

            textZInv.setText(String.valueOf(zSliceInv + 1));
            updateClipSliceZInv();

            if (!sliderZInv.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == sliderA) {
            aSlice = sliderA.getValue() - 1;

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderA" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderA" + clipCount);
                recordEventName = false;
            }

            textA.setText(String.valueOf(aSlice + 1));
            updateClipSliceA();

            if (!sliderA.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == clipSliderStatic) {
            sSlice = clipSliderStatic.getValue() - 1;

            if (sSlice > sSliceInv) {
                sSlice = sSliceInv;
                clipSliderStatic.setValue(sSlice);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderStatic" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderStatic" + clipCount);
                recordEventName = false;
            }

            textStatic.setText(String.valueOf(sSlice + 1));
            updateClipSliceStatic();

            if (!clipSliderStatic.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }
        } else if (source == clipSliderStaticInv) {
            sSliceInv = clipSliderStaticInv.getValue() - 1;

            if (sSliceInv < sSlice) {
                sSliceInv = sSlice;
                clipSliderStaticInv.setValue(sSlice);
            }

            if (myMouseDialog.isRecording() && recordEventName) {
                clipEvents.setName("clipSliderStaticInv" + clipCount);
                myMouseDialog.listModel.addElement("clipSliderStaticInv" + clipCount);
                recordEventName = false;
            }

            textStaticInv.setText(String.valueOf(sSliceInv + 1));
            updateClipSliceStaticInv();

            if (!clipSliderStaticInv.getValueIsAdjusting()) {
                updateVolumeRenderClipPlane();
            }

        }

        if (myMouseDialog.isRecording()) {
            clipEvents.add(e, renderBase.getSceneState());
        }
    }

    /**
     * Swap ModelClip Branch group between the 6 clipping plane and the arbitray clipping plane.
     *
     * @param  flag  <code>true</code> enable the 6 clipping planes, <code>false</code> enable the arbitrary clipping
     *               plane.
     */
    public void swapModelClipBG(boolean flag) {

        if ( (mcArbi == null) || (mc == null) || (mcStatic == null) )
        {
            return;
        }

        if (flag) { // true,  enable 6 clipping plane.
            is6PlaneClipping = true;
            mcArbi.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
            mc.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
            mcArbiExtendBG.detach();

            if (!mcExtendBG.isLive()) {
                mcBG.addChild(mcExtendBG);
            }
            mc.addScope(((SurfaceRender) renderBase).getVolRenderBG());
            disableStaticClipping();
        } else { // false. enable arbitrary clipping plane.
            is6PlaneClipping = false;
            mcArbi.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
            mc.removeScope(((SurfaceRender) renderBase).getVolRenderBG());
            mcExtendBG.detach();

            if (!mcArbiExtendBG.isLive()) {
                mcArbiTG.addChild(mcArbiExtendBG);
            }

            mcArbi.addScope(((SurfaceRender) renderBase).getVolRenderBG());
            disableStaticClipping();
        }
    }


    /**
     * Needed for this to implement MouseBehaviorCallback.
     *
     * @param  type       transformation type.
     * @param  transform  transformation matrix.
     */
    public void transformChanged(int type, Transform3D transform) {
        JPanelMouse myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();

        if (myMouseDialog.isPlaying() && !isClipArbiPicked && (MouseBehaviorCallback.ROTATE == type)) {
            return;
        }

        if (myMouseDialog.isRecording() && (isClipArbiPicked == true) && (MouseBehaviorCallback.ROTATE == type)) {
            isTransformChanged = true;

            if (recordEventName) {
                myMouseDialog.listModel.addElement("ArbitraryRotation" + rotationCount);
                recordEventName = false;
            }
        }

        if (isClipArbiPicked) {
            updateClipSliceAwithRotate(transform);
        }
    }

    /**
     * Undo the crop of the volume.
     */
    public void undoCrop() {
        ModelImage kImageA = ((SurfaceRender) renderBase).getImageA();
        ModelImage kImageB = ((SurfaceRender) renderBase).getImageB();

        if (m_aiImageA_backup == null) {
            return;
        }

        /* The extents of the ModelImages: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;
        int iSize = iXBound * iYBound * iZBound;

        /* Reset the ModelImage to the original values */
        for (int iY = 0; iY < iYBound; iY++) {
            int iIndexY = iY * iXBound;

            for (int iX = 0; iX < iXBound; iX++) {

                for (int iZ = 0; iZ < iZBound; iZ++) {
                    int iIndexZ = iZ * iSliceSize;
                    int iIndex = iIndexZ + iIndexY + iX;

                    if (kImageA.isColorImage()) {

                        for (int iColor = 0; iColor < 4; iColor++) {
                            kImageA.set((iIndex * 4) + iColor, m_aiImageA_backup[(iIndex * 4) + iColor]);
                        }
                    } else {
                        kImageA.set(iIndex, m_aiImageA_backup[iIndex]);
                    }

                    if (kImageB != null) {

                        if (kImageB.isColorImage()) {

                            for (int iColor = 0; iColor < 4; iColor++) {
                                kImageB.set((iIndex * 4) + iColor, m_aiImageB_backup[(iIndex * 4) + iColor]);
                            }
                        } else {
                            kImageB.set(iIndex, m_aiImageB_backup[iIndex]);
                        }
                    }
                }
            }
        }

        ((SurfaceRender) renderBase).updateData();
        ((SurfaceRender) renderBase).updateImages();
    }

    /**
     * Upadate six clipping planes' plane equation.
     */
    public void updateClipPlanesEqn() {
        eqnPlanes[0] = eqnX;
        eqnPlanes[1] = eqnXInv;
        eqnPlanes[2] = eqnY;
        eqnPlanes[3] = eqnYInv;
        eqnPlanes[4] = eqnZ;
        eqnPlanes[5] = eqnZInv;

        eqnPlanesArbi[0] = eqnA;
        eqnPlanesArbi[1] = eqnXInv;
        eqnPlanesArbi[2] = eqnY;
        eqnPlanesArbi[3] = eqnYInv;
        eqnPlanesArbi[4] = eqnZ;
        eqnPlanesArbi[5] = eqnZInv;

        eqnPlanesStatic[0] = eqnX;
        eqnPlanesStatic[1] = eqnXInv;
        eqnPlanesStatic[2] = eqnY;
        eqnPlanesStatic[3] = eqnYInv;
        eqnPlanesStatic[4] = eqnZ;
        eqnPlanesStatic[5] = eqnZInv;
    }

    /**
     * Update clipping planes.
     */
    public void updateClipSlice() {
        updateClipSliceA();
        updateClipSliceX();
        updateClipSliceXInv();
        updateClipSliceY();
        updateClipSliceYInv();
        updateClipSliceZ();
        updateClipSliceZInv();
        updateVolumeRenderClipPlane();
    }

    /**
     * Sets new frame around arbitrary clip plane slice based on the new position.
     */
    public void updateClipSliceA() {

        if (getAVisible()) {

            if ( (mcArbi != null) && (mc != null) && (mcStatic != null) )
            {
                
                if (rotationAxis != null) {
                    mcArbiTrans3D.set(rotationAxis);
                    arbiTrans3d.set(rotationAxis);
                }
                
                mcArbiTG.setTransform(mcArbiTrans3D);
                arbiTG.setTransform(arbiTrans3d);
                
                
                eqnA = new Vector4d(1.0, 0.0, 0.0, (xBox - (2 * ((float) aSlice / (xDim - 1)) * xBox) - 0.001));
                updateClipPlanesEqn();
                mcArbi.setPlanes(eqnPlanesArbi);
                mcArbi.setEnable(0, true);
                mcArbi.setInfluencingBounds(renderBase.getBound());
                
                clipSliceA.setSlices(-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox), maxBox, maxBox,
                                     ViewJComponentBoxSlice.A_CLIPSLICE);
                
                clipSliceAIndicator.setSlices(-maxBox + (2 * ((float) aSlice / (maxDim - 1)) * maxBox), maxBox, maxBox,
                                              ViewJComponentBoxSlice.A_CLIPSLICE);
                
                
                displayAClipPlanePts();
            }
        }
    }

    /**
     * Update arbitrary clip plane's transform.
     *
     * @param  transform  The current transform with the mouse moves.
     */
    public void updateClipSliceAwithRotate(Transform3D transform) {
        javax.vecmath.Matrix4f rotationMatrix = new javax.vecmath.Matrix4f();

        rotationAxis = new AxisAngle4f();
        transform.get(rotationMatrix);
        rotationAxis.set(rotationMatrix);
        axisX = rotationAxis.x;
        axisY = rotationAxis.y;
        axisZ = rotationAxis.z;
        axisAngle = rotationAxis.angle;

        mcArbiTrans3D.set(rotationAxis);
        mcArbiTG.setTransform(mcArbiTrans3D);
        arbiTrans3d.set(rotationAxis);
        arbiTG.setTransform(arbiTrans3d);

        // displayIntersection();
        displayAClipPlanePts();
    }

    /**
     * Sets new frame around clip plane slice -z based on the new position.
     */
    public void updateClipSliceStatic() {

        if (getStaticVisible()) {
            if ( mcStatic != null )
            {
                eqnZInv = new Vector4d(0.0, 0.0, 1.0, (2 * ((float) sSlice / (zDim - 1)) * zBox) - zBox - 0.001);
                updateClipPlanesEqn();
                mcStatic.setPlanes(eqnPlanesStatic);
                mcStatic.setEnable(5, true);
                mcStatic.setInfluencingBounds(renderBase.getBound());
                
                clipSliceStatic.setSlices(xBox * 0.5f, yBox * 0.5f,
                                          zBox - (2 * ((float) sSlice / (zDim - 1)) * zBox) + 0.001f,
                                          ViewJComponentBoxSlice.S_CLIPSLICE);


                Shape3D shape = new Shape3D(clipSliceStatic, null);

                shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
                shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
                shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

                try {
                    objClipSliceStatic_BG.addChild(shape);
                } catch (RestrictedAccessException error) { }

                displaySClipPlanePts();
            }
        }
    }

    /**
     * Sets new frame around clip plane slice -z based on the new position.
     */
    public void updateClipSliceStaticInv() {

        if (getStaticInvVisible()) {
            if ( mcStatic != null )
            {
                eqnZ = new Vector4d(0.0, 0.0, -1.0, zBox - (2 * ((float) sSliceInv / (zDim - 1)) * zBox) - 0.001);
                updateClipPlanesEqn();
                mcStatic.setPlanes(eqnPlanesStatic);
                mcStatic.setEnable(4, true);
                mcStatic.setInfluencingBounds(renderBase.getBound());

                clipSliceStaticInv.setSlices(xBox * 0.5f, yBox * 0.5f,
                                             zBox - (2 * ((float) sSliceInv / (zDim - 1)) * zBox) + 0.001f,
                                             ViewJComponentBoxSlice.S_CLIPSLICE_NEG);

                Shape3D shape = new Shape3D(clipSliceStaticInv, null);

                shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
                shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
                shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

                try {
                    objClipSliceStaticInv_BG.addChild(shape);
                } catch (RestrictedAccessException error) { }
                // displaySClipPlanePts();
            }
        }
    }

    /**
     * Sets new frame around clip plane slice x based on the new position.
     */
    public void updateClipSliceX() {
        if ( mc == null )
        {
            return;
        }

        if (getXVisible()) {
            eqnX = new Vector4d(1.0, 0.0, 0.0, xBox - (2 * ((float) xSlice / (xDim - 1)) * xBox) - 0.001);
            updateClipPlanesEqn();
            //System.err.println( eqnPlanes[0] + " " + eqnPlanes[1] + " " + eqnPlanes[2] + " " + eqnPlanes[3]);
            mc.setPlanes(eqnPlanes);
            mc.setEnable(0, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceX.setSlices(-xBox + (2 * ((float) xSlice / (xDim - 1)) * xBox), yBox, zBox,
                                 ViewJComponentBoxSlice.X_CLIPSLICE);

            Shape3D shape = new Shape3D(clipSliceX, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceX_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Sets new frame around clip plane slice -x based on the new position.
     */
    public void updateClipSliceXInv() {
        if ( mc == null )
        {
            return;
        }

        if (getXVisibleInv()) {
            eqnXInv = new Vector4d(-1.0, 0.0, 0.0, -xBox + (2 * ((float) xSliceInv / (xDim - 1)) * xBox) - 0.001);
            updateClipPlanesEqn();
            mc.setPlanes(eqnPlanes);
            mc.setEnable(1, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceXInv.setSlices(-xBox + (2 * ((float) xSliceInv / (xDim - 1)) * xBox), yBox, zBox,
                                    ViewJComponentBoxSlice.X_CLIPSLICE_NEG);

            Shape3D shape = new Shape3D(clipSliceXInv, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceXInv_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Sets new frame around clip plane slice y based on the new position.
     */
    public void updateClipSliceY() {
        if ( mc == null )
        {
            return;
        }

        if (getYVisible()) {
            eqnY = new Vector4d(0.0, -1.0, 0.0, yBox - (2 * ((float) ySlice / (yDim - 1)) * yBox) - 0.001);
            updateClipPlanesEqn();
            mc.setPlanes(eqnPlanes);
            mc.setEnable(2, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceY.setSlices(xBox, yBox - (2 * ((float) ySlice / (yDim - 1)) * yBox), zBox,
                                 ViewJComponentBoxSlice.Y_CLIPSLICE);

            Shape3D shape = new Shape3D(clipSliceY, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceY_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Sets new frame around clip plane slice -y based on the new position.
     */
    public void updateClipSliceYInv() {
        if ( mc == null )
        {
            return;
        }

        if (getYVisibleInv()) {
            eqnYInv = new Vector4d(0.0, 1.0, 0.0, -yBox + (2 * ((float) ySliceInv / (yDim - 1)) * yBox) - 0.001);
            updateClipPlanesEqn();
            mc.setPlanes(eqnPlanes);
            mc.setEnable(3, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceYInv.setSlices(xBox, yBox - (2 * ((float) ySliceInv / (yDim - 1)) * yBox), zBox,
                                    ViewJComponentBoxSlice.Y_CLIPSLICE_NEG);

            Shape3D shape = new Shape3D(clipSliceYInv, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceYInv_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Sets new frame around clip plane slice z based on the new position.
     */
    public void updateClipSliceZ() {
        if ( mc == null )
        {
            return;
        }

        if (getZVisible()) {
            eqnZ = new Vector4d(0.0, 0.0, -1.0, zBox - (2 * ((float) zSlice / (zDim - 1)) * zBox) - 0.001);
            updateClipPlanesEqn();
            mc.setPlanes(eqnPlanes);
            mc.setEnable(4, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceZ.setSlices(xBox, yBox, zBox - (2 * ((float) zSlice / (zDim - 1)) * zBox),
                                 ViewJComponentBoxSlice.Z_CLIPSLICE);

            Shape3D shape = new Shape3D(clipSliceZ, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceZ_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Sets new frame around clip plane slice -z based on the new position.
     */
    public void updateClipSliceZInv() {
        if ( mc == null )
        {
            return;
        }

        if (getZVisibleInv()) {
            eqnZInv = new Vector4d(0.0, 0.0, 1.0, (2 * ((float) zSliceInv / (zDim - 1)) * zBox) - zBox - 0.05);
            updateClipPlanesEqn();
            mc.setPlanes(eqnPlanes);
            mc.setEnable(5, true);
            mc.setInfluencingBounds(renderBase.getBound());

            clipSliceZInv.setSlices(xBox, yBox, zBox - (2 * ((float) zSliceInv / (zDim - 1)) * zBox) + 0.05f,
                                    ViewJComponentBoxSlice.Z_CLIPSLICE_NEG);

            Shape3D shape = new Shape3D(clipSliceZInv, null);

            shape.setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            shape.setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            shape.setCapability(Shape3D.ENABLE_PICK_REPORTING);

            try {
                objClipSliceZInv_BG.addChild(shape);
            } catch (RestrictedAccessException error) { }
        }
    }

    /**
     * Accessor that closing the mouse recorder window frame.
     *
     * @param  e  Window event.
     */
    public void windowClosing(WindowEvent e) {
        disableClipPlanes();
        disableClipPlanesArbi();
    }

    /**
     * Called by the TextureSculptor or VolumeSculptor objects. The function stores the original volume data back to the
     * original values in the m_aiImage_backup data members.
     *
     * @param  kImageA  image A reference.
     * @param  kImageB  image B reference.
     */
    protected void backupImage(ModelImage kImageA, ModelImage kImageB) {

        /* The size of the ModelImage for indexing: */
        int iXBound = kImageA.getExtents()[0];
        int iYBound = kImageA.getExtents()[1];
        int iZBound = kImageA.getExtents()[2];
        int iSliceSize = iXBound * iYBound;

        int iColor = 1;

        if (kImageA.isColorImage()) {
            iColor = 4;
        }

        if (m_aiImageA_backup == null) {
            m_aiImageA_backup = new int[iXBound * iYBound * iZBound * iColor];

            if (kImageB != null) {
                iColor = 1;

                if (kImageB.isColorImage()) {
                    iColor = 4;
                }

                m_aiImageB_backup = new int[iXBound * iYBound * iZBound * iColor];
            }
        }

        for (int iZ = 0; iZ < iZBound; iZ++) {

            for (int iY = 0; iY < iYBound; iY++) {

                for (int iX = 0; iX < iXBound; iX++) {
                    int iIndex = (iZ * iSliceSize) + (iY * iXBound) + iX;

                    /* Store the volume data in the backup */
                    if (kImageA.isColorImage()) {

                        for (int iC = 0; iC < iColor; iC++) {
                            m_aiImageA_backup[(iIndex * iColor) + iC] = kImageA.getInt((iIndex * iColor) + iC);
                        }
                    } else {
                        m_aiImageA_backup[iIndex] = kImageA.getInt(iIndex);
                    }

                    if (kImageB != null) {

                        if (kImageB.isColorImage()) {

                            for (int iC = 0; iC < iColor; iC++) {
                                m_aiImageB_backup[(iIndex * iColor) + iC] = kImageB.getInt((iIndex * iColor) + iC);
                            }
                        } else {
                            m_aiImageB_backup[iIndex] = kImageB.getInt(iIndex);
                        }
                    }
                }
            }
        }
    }

    /**
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  color button.
     * @param  color   color reference.
     */
    protected void setBoxColor(JButton button, Color color) {

        if (button == colorButtonX) {
            setClipSliceXColor(color);
        } else if (button == colorButtonY) {
            setClipSliceYColor(color);
        } else if (button == colorButtonZ) {
            setClipSliceZColor(color);
        } else if (button == colorButtonXInv) {
            setClipSliceXInvColor(color);
        } else if (button == colorButtonYInv) {
            setClipSliceYInvColor(color);
        } else if (button == colorButtonZInv) {
            setClipSliceZInvColor(color);
        } else if (button == colorButtonA) {
            setClipSliceAColor(color);
        } else if (button == colorButtonStatic) {
            setClipSliceSColor(color);
        } else if (button == colorButtonStaticInv) {
            setClipSliceSInvColor(color);
        }
    }

    /**
     * Helper method that adds components to the control panel for the grid bag layout.
     *
     * @param  panel  control panel.
     * @param  c      Component added to the control panel.
     * @param  gbc    GridBagConstraints of added component.
     * @param  x      Gridx location
     * @param  y      Gridy location
     * @param  w      Gridwidth
     * @param  h      Gridheight
     */
    private void addControlPanel(JPanel panel, Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        panel.add(c, gbc);
    }

    /**
     * Draw the red sphere with the given coordinate.
     *
     * @param  index  sphere index
     * @param  x      float x position
     * @param  y      float y position
     * @param  z      float z position
     */
    private void drawSphere(int index, float x, float y, float z) {
        Transform3D t = new Transform3D();

        t.setTranslation(new javax.vecmath.Vector3f(x, y, z));
        m_akLightTransformGroup[index].setTransform(t);
    }

    /**
     * Initialize the red sphere image scene graph structure.
     */
    private void initSphereBranch() {
        m_akLightBG = new BranchGroup[iNumLights];
        m_akLightSpheres = new Sphere[iNumLights];
        m_akLightMaterials = new Material[iNumLights];
        m_akLightTransformGroup = new TransformGroup[iNumLights];

        m_kLightRoot = new BranchGroup();
        m_kLightRoot.setCapability(BranchGroup.ALLOW_DETACH);
        m_kLightRoot.setCapability(BranchGroup.ALLOW_CHILDREN_READ);
        m_kLightRoot.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

        if (!m_kLightRoot.isLive()) {
            renderBase.getSceneRootTG().addChild(m_kLightRoot);
        }

        Color3f orangeColor = new Color3f(new Color(225, 50, 0));
        Color3f ambientColor = new Color3f(1.0f, 0.0f, 0.0f);
        Color3f emissiveColor = new Color3f(0.0f, 0.0f, 0.0f);
        Color3f sepcualarColor = new Color3f(1.0f, 1.0f, 1.0f);
        Color3f diffuseColor = new Color3f(1.0f, 0.0f, 0.0f);

        intersectionPts = new Point3f[iNumLights];

        for (int i = 0; i < iNumLights; i++) {
            m_akLightBG[i] = new BranchGroup();
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_DETACH);
            m_akLightBG[i].setCapability(Group.ALLOW_CHILDREN_EXTEND);
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            m_akLightBG[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);

            // Create appearance for display of light.
            Appearance kAppearance = new Appearance();

            m_akLightMaterials[i] = new Material(emissiveColor, emissiveColor, orangeColor, sepcualarColor, 50.0f);
            m_akLightMaterials[i].setCapability(Material.ALLOW_COMPONENT_READ);
            m_akLightMaterials[i].setCapability(Material.ALLOW_COMPONENT_WRITE);
            kAppearance.setMaterial(m_akLightMaterials[i]);

            // Create a transform group node to scale and position the object.
            Transform3D t = new Transform3D();

            t.setTranslation(new javax.vecmath.Vector3f(-999, -999, -999));

            m_akLightTransformGroup[i] = new TransformGroup(t);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_TRANSFORM_READ);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_LOCAL_TO_VWORLD_READ);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_CHILDREN_READ);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
            m_akLightTransformGroup[i].setCapability(BranchGroup.ALLOW_DETACH);
            m_akLightTransformGroup[i].setCapability(TransformGroup.ALLOW_CHILDREN_EXTEND);
            m_akLightSpheres[i] = new Sphere(.02f, kAppearance);
            m_akLightSpheres[i].setCapability(Sphere.ENABLE_APPEARANCE_MODIFY);
            m_akLightSpheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_READ);
            m_akLightSpheres[i].getShape().setCapability(Shape3D.ALLOW_APPEARANCE_WRITE);
            m_akLightSpheres[i].getShape().setAppearanceOverrideEnable(true);
            m_akLightSpheres[i].getShape().setCapability(Geometry.ALLOW_INTERSECT);

            m_akLightBG[i].addChild(m_akLightSpheres[i]);
            m_akLightTransformGroup[i].addChild(m_akLightBG[i]);
            m_kLightRoot.addChild(m_akLightTransformGroup[i]);

            intersectionPts[i] = new Point3f();
        }

        AclipPlanePts = new javax.vecmath.Vector4f[4];

        for (int i = 0; i < 4; i++) {
            AclipPlanePts[i] = new javax.vecmath.Vector4f();
        }

    }

    /**
     * Transform a point rotation inverse to the volume rotation.
     *
     * @param  vIn  point position in vector
     */
    private void transform(javax.vecmath.Vector4f vIn) {
        float x, y, z;

        x = vIn.x;
        y = vIn.y;
        z = vIn.z;

        Transform3D result = new Transform3D();
        renderBase.getSceneRootTG().getTransform(result);
        result.invert();
        result.transform(vIn);
        
        
/*
        Transform3D trans = new Transform3D();
        // Set the invert of the scale: transformNode3d.setScale( 0.45f );
        trans.setScale(1 / 0.45f);
        trans.setTranslation(new javax.vecmath.Vector3f(-x, -y, -z));

        Transform3D result = new Transform3D();
        Vector3d euler = new Vector3d();

        renderBase.getSceneRootTG().getTransform(result);

        // Get the Euler angle rotation component from the Quaternion.
        Quat4d q = new Quat4d();

        result.get(q);
        QuatToEuler(q, euler);
        result.setIdentity();
        result.setEuler(new Vector3d(-euler.x, -euler.y, -euler.z));
        trans.mul(result);
        trans.setTranslation(new javax.vecmath.Vector3f(x, y, z));

        trans.transform(vIn);
        */
    }

    /**
     * Update the volume render's clip plane with all new positions.
     */
    private void updateVolumeRenderClipPlane() {
        if (rayBasedRender != null) {
            rayBasedRender.updateImages(true);
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Does nothing.
         *
         * @param  e  event handler
         */
        public void actionPerformed(ActionEvent e) { }
    }


    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7735741437177785055L;

        /**
         * Wrapper to repaint the panel.
         *
         * @param  g  graphics reference.
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }


    /**
     * Pick up the selected color and call method to change the VOI color.
     */
    class OkColorListener implements ActionListener {

        /** Button reference. */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  button reference.
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and VOI color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setBoxColor(button, color);
        }
    }

}
