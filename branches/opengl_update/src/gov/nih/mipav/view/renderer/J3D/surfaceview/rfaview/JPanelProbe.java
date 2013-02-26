package gov.nih.mipav.view.renderer.J3D.surfaceview.rfaview;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.media.j3d.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Dialog to turn slices bounding box of surface renderer on and off, and to change the color of the frame. This dialog
 * also control the X, Y, Z slices movements.
 *
 * @author  Ruida Cheng
 */
public class JPanelProbe extends JPanelRendererJ3D implements ChangeListener, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2467552118438070016L;

    /** cursor default nevigation mode. */
    private static int NONE = 1;

    /** cursor probe nevigation mode. */
    private static int PROBEMOVE = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton bonePresetButton;

    /** burning point back face culling check box. */
    private JCheckBox burnBackFaceCB;

    /** Burn base reference. */
    private BurnBase burnBase;

    /** burning point clipping check box. */
    private JCheckBox burnClipCB;

    /** Current burning point index being hightlighted. */
    private int burnIndex = -1;

    /** Burning point coordinate vector in world coordinate system. */
    private Vector3f burningPtTransVector = new Vector3f();

    /** Burn list that hold the burning point names. */
    private JList burnList;

    /** Calculat button. */
    private JButton calcButton;

    /** Burning point coordinate. */
    private Point3f center;

    /** Burn control panel. */

    /** burning point center label. */
    private JLabel centerLabel;

    /** Burning surface being clipped or not. */
    private boolean clipping;

    /** Burning point color button. */
    private JButton colorButton;

    /** Color chooser when the user changes the color of the surface. */
    private ViewJColorChooser colorChooser;

    /** Burning point color label. */
    private JLabel colorLabel;

    /** Burning surface being culled ( back face bulling ) or not. */
    private boolean culling;

    /** curreent cursor nevigation mode. */
    private int cursorMode = NONE;

    /** Volume difference label for the current treatment. */
    private JLabel diffVolumeLabel;

    /** Volume difference text. */
    private JTextField diffVolumeText;

    /** Associate structure of the scroll pane to resize the panel properly. */
    private DrawingPane drawingPane;

    /** End point coordinate vector in the world coordinate system. */
    private Vector3f endPtTransVector = new Vector3f();

    /** entry point label. */
    private JLabel entryPointLabel;

    /** Panel that hold the entry point roation check box. */
    private JPanel entryPointPanel;

    /** entry point rotation check box. */
    private JCheckBox entryPointRotationCB;

    /** Liver segmentation preset opacity transform function control button. */
    private JButton liverPresetButton;

    /** Load button for the opacity transform function. */
    private JButton loadVisualizationButton;

    /** DOCUMENT ME! */
    private JLabel m_kProbeTwistLabel;

    /** Rotation/Twist around the direction of the probe:. */
    private JSlider m_kProbeTwistSlider;

    /** The maximum of the fram box values. */
    private float maxBox;

    /** Treatment information to be used if the user wants to burn without any target surface. */
    private TreatmentInformation noTargetTreatment;

    /** Burning point opacity. */
    private float opacity;

    /** The opacity slider label. */
    private JLabel opacityLabel;

    /** burning point surface opacity slider. */
    private JSlider opacitySlider;

    /** burning point surface opacity slider label. */
    private JLabel[] opacitySliderLabels;

    /** Burning point parameter dialog to change the burning attributes, such as diameter. */
    //private JDialogBurnParameter paramDialog;

    /** Volume view frame work reference. */
    private ViewJFrameVolumeView parentFrame;

    /** Probe reference. */
    private Probe probe;

    /** Probes list. */
    private JList probeList;

    /** Reset probe nevigation mode. */
    private JToggleButton probeResetButton;

    /** Control the tri-planar probe nevigation mode. */
    private JToggleButton probeTargetButton;

    /** Buring point sphere diameter. */
    //private Point3f radius;

    /** burning point diameter label. */
    private JLabel radiusLabel;

    /** result label to show the bone, vasculature detection information message. */
    private JLabel resultLabel;

    /** Save button for the opacity tranform function. */
    private JButton saveVisualizationButton;

    /** Scroll pane that control the resizing of the panel. */
    private JScrollPane scroller;

    /** burning point semi axis X label. */
    private JLabel semiXLabel;

    /** burning point semi axis X text field. */
    private JTextField semiXText;

    /** burning point semi axis Y label. */
    private JLabel semiYLabel;

    /** burning point semi axis Y text field. */
    private JTextField semiYText;

    /** burning point semi axis Z label. */
    private JLabel semiZLabel;

    /** burning point semi axis Z text field. */
    private JTextField semiZText;

    /** Showing the burning process or not. */
    //private boolean showBurning;

    /** Flag indicates the show burning text label or not. */
    private boolean showBurnLabels = false;

    /** Show the burning text label checkbox. */
    private JCheckBox showBurnLabelsCB;

    /** Skin segmentation preset opacity transform function control button. */
    private JButton skinPresetButton;

    /** The probe X slider that control the probe push and pull movements. */
    private JSlider sliderX;

    /** Whether to snap to center of a target surface when it is selected; defaults to true. */
    private boolean snapToTarget = true;

    /** Checkbox indicating whether to snap to center of a target surface when it is selected. */
    private JCheckBox snapToTargetCB;

    /** Start point ( invisible ) coordinate vector in world coordinate system. */
    private Vector3f startPtTransVector = new Vector3f();

    /** The main tabbed pane in the volume view frame. */
    private JTabbedPane tabbedPane;

    /** The main tab contrl panel. */
    private JPanel tabPanel;

    /** Surface target list (tumors). */
    private JList targetList;

    /** Label containing the volume of the current target surface. */
    private JTextField targetVolumeText;

    /** X label for the center coordinate of the current target surface. */
    private JTextField targetXText;

    /** Y label for the center coordinate of the current target surface. */
    private JTextField targetYText;

    /** Z label for the center coordinate of the current target surface. */
    private JTextField targetZText;

    /** Combo box for indicating the length of the probe tip. Currently used just by CoolTip probe. */
    private JComboBox tipLengthCB;

    /** Tool bar. */
    private JToolBar toolBar;

    /** Burning points total volume label. */
    private JLabel totalVolumeLabel;

    /** Burning point total volume text. */
    private JTextField totalVolumeText;

    /** Control the tri-planar default view nevigation mode. */
    private JToggleButton traverseButton;

    /** List of treatment sets. */
    private Vector<TreatmentInformation> treatmentVector; 

    /** burning point diameter text. */
    // private JTextField diameterText;

    /** buring point volume label. */
    private JLabel volumeLabel;

    /** burning pont volume text box. */
    private JTextField volumeText;

    /** Parent frame box value. */
    private float xBox, yBox, zBox;

    /** burning point X, Y, Z coordinate label. */
    private JLabel xLabel, yLabel, zLabel;

    /** entry point X, Y, Z coordinate label. */
    private JLabel xLabelEntry, yLabelEntry, zLabelEntry;

    /** The resolutions of the (possibly respampled) image (the one shown in the renderer). */
    private float xRes, yRes, zRes;

    /** slider that controls the probe moves along the x direction. */
    private JSlider xSliderS;

    /** burning point X, Y, Z coordinate text box. */
    private JTextField xText, yText, zText;

    /** entry point X, Y, Z coordinate text box. */
    private JTextField xTextEntry, yTextEntry, zTextEntry;

    /** slider that controls the probe moves along the y direction. */
    private JSlider ySliderS;

    /** slider that controls the probe moves along the z direction. */
    private JSlider zSliderS;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to initialize the probe control panel.
     *
     * @param  parent        SurfaceRender reference.
     * @param  _parentFrame  ViewJFrameVolumeView reference.
     * @param  xBox          DOCUMENT ME!
     * @param  yBox          DOCUMENT ME!
     * @param  zBox          DOCUMENT ME!
     */
    public JPanelProbe(SurfaceRender parent, ViewJFrameBase _parentFrame, float xBox, float yBox, float zBox) {
        super(parent);
        parentFrame = (ViewJFrameVolumeView) _parentFrame;
        init();
        probe = new Probe(parent);
        burnBase = new BurnBase(parent, this);
        updateProbeList();
        updateTargetList();
        targetList.addListSelectionListener(this);
        treatmentVector = new Vector<TreatmentInformation>();
        noTargetTreatment = new TreatmentInformation();
        this.xBox = xBox;
        this.yBox = yBox;
        this.zBox = zBox;
        maxBox = Math.max(xBox, Math.max(yBox, zBox));
        xRes = parentFrame.getImageA().getFileInfo(0).getResolutions()[0];
        yRes = parentFrame.getImageA().getFileInfo(0).getResolutions()[1];
        zRes = parentFrame.getImageA().getFileInfo(0).getResolutions()[2];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Action perform event function that handle all the commands in the probe panel.
     *
     * @param  event  Action Event
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        //Object source = event.getSource();

        if (command.equals("calcVolume")) {

            // /
            burnBase.calcVolume(getCurSurface(), getCurTreatment());

            // / assumes that the units are the same in all dims
            String unitStr = Unit.getUnitFromLegacyNum(parentFrame.getImageOriginal().getFileInfo(0).getUnitsOfMeasure(0)).getAbbrev();

            totalVolumeText.setText(" " + MipavUtil.makeFloatString(burnBase.getVolume(), 3) + " " + unitStr + "^3");
            diffVolumeText.setText(" " + MipavUtil.makeFloatString(burnBase.getDiffVolume(), 3) + " " + unitStr + "^3");

            // save total volume in treatment info
            getCurTreatment().setTotalVolume(burnBase.getVolume());
            getCurTreatment().setDiffVolume(burnBase.getDiffVolume());
        } else if (command.equals("traverse")) {
            cursorMode = NONE;
            parentFrame.disableTargetPointPicking();
        } else if (command.equals("ProbeTargetPoint")) {
            cursorMode = PROBEMOVE;
            parentFrame.enableTargetPointPicking();
        } else if (command.equals("ProbeReset")) {
            probe.resetProbeTransform();
        } else if (command.equals("Navigation")) {
            probe.navigation();
        } else if (command.equals("addTarget")) {

            // callback to updateTargetList() updates treatmentVector
            ((SurfaceRender) renderBase).getSurfaceDialog().addSurface();
        } else if (command.equals("removeTarget")) {
            //int surIndex = targetList.getSelectedIndex();

            // remove all burns attached to this target
            removeAllBurn();

            // callback to updateTargetList() updates treatmentVector
            ((SurfaceRender) renderBase).getSurfaceDialog().removeSurface();

            if (treatmentVector.size() > 0) {

                // explicitly done since removal doesn't seem to generate ListSelectionEvents
                updateTreatmentPanel(true);
            } else {
                enableBurnTreatmentComponents(false);
            }
        } else if (command.equals("removeBurn")) {
            removeBurn();
        } else if (command.equals("removeAllBurn")) {
            removeAllBurn();
        } else if (command.equals("entryPointRotation")) {
            sliderX.setValue(0);
            probe.enableEntryPointRotation(entryPointRotationCB.isSelected());
        } else if (command.equals("Burn")) {

            if (probe.isProbeRootParentBGLive()) {
                startBurn();
            }
        } else if (command.equals("burnPacking")) {
            startBurnPacking();
        } else if (command.equals("backface")) {

            // operates on all burns attached to the current target surface treatment
            for (int i = 0; i < getCurTreatment().getNumBurns(); i++) {
                BranchGroup root = getCurTreatment().getBurn(i).burnBG;

                if (root != null) {
                    root.setCollidable(burnBackFaceCB.isSelected());

                    Shape3D shape = (Shape3D) (((BranchGroup) (root.getChild(0))).getChild(0));

                    if (burnBackFaceCB.isSelected()) {
                        shape.getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_BACK);
                    } else {
                        shape.getAppearance().getPolygonAttributes().setCullFace(PolygonAttributes.CULL_NONE);
                    }

                    getCurTreatment().getBurn(i).culling = burnBackFaceCB.isSelected();
                }
            }
        } else if (command.equals("Clipping") && ((SurfaceRender) renderBase).getDisplayMode3D()) {

            // operates on all burns attached to the current target surface treatment
            for (int i = 0; i < getCurTreatment().getNumBurns(); i++) {
                BranchGroup root = getCurTreatment().getBurn(i).burnBG;

                if (root != null) {
                    root.setAlternateCollisionTarget(burnClipCB.isSelected());

                    if (burnClipCB.isSelected()) {
                        ((SurfaceRender) renderBase).getClipDialog().addToModelClip(root);
                    } else {
                        ((SurfaceRender) renderBase).getClipDialog().removeFromModelClip(root);
                    }

                    getCurTreatment().getBurn(i).clipping = burnClipCB.isSelected();
                }
            }
        } else if (command.equals("ChangeColor")) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick burn color", new OkColorListener(),
                                                 new CancelListener());
        } else if (command.equals("SkinPreset")) {
            // load skin presets
            /* TransferFunction lutFunc = new TransferFunction();
             * lutFunc.addPoint( (float) parentFrame.getImageA().getMin(), 255 ); lutFunc.addPoint( -206, 255 );
             * lutFunc.addPoint( 356, 0 ); lutFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0 );
             *
             * TransferFunction opacFunc = new TransferFunction(); opacFunc.addPoint( (float)
             * parentFrame.getImageA().getMin(), 100 ); opacFunc.addPoint( -497, 100 ); opacFunc.addPoint( (float)
             * parentFrame.getImageA().getMax() - 1, 0 ); opacFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0
             * );
             *
             * loadVisualizationSettings( lutFunc, opacFunc );*/

            float[] xLut = new float[4];
            float[] yLut = new float[4];

            xLut[0] = (float) parentFrame.getImageA().getMin();
            yLut[0] = 255;

            xLut[1] = -206;
            yLut[1] = 255;

            xLut[2] = 356;
            yLut[2] = 0;

            xLut[3] = (float) parentFrame.getImageA().getMax();
            yLut[3] = 0;

            TransferFunction lutTrans = new TransferFunction();

            lutTrans.importArrays(xLut, yLut, 4);

            float[] xOp = new float[4];
            float[] yOp = new float[4];

            xOp[0] = (float) parentFrame.getImageA().getMin();
            yOp[0] = 100;

            xOp[1] = -497;
            yOp[1] = 100;

            xOp[2] = (float) parentFrame.getImageA().getMax() - 1;
            yOp[2] = 0;

            xOp[3] = (float) parentFrame.getImageA().getMax();
            yOp[3] = 0;

            TransferFunction opacTrans = new TransferFunction();

            opacTrans.importArrays(xOp, yOp, 4);

            loadVisualizationSettings(lutTrans, opacTrans);
        } else if (command.equals("LiverPreset")) {
            // load liver presets
            /* TransferFunction lutFunc = new TransferFunction();
             * lutFunc.addPoint( (float) parentFrame.getImageA().getMin(), 255 ); lutFunc.addPoint( -206, 255 );
             * lutFunc.addPoint( 356, 0 ); lutFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0 );
             *
             * TransferFunction opacFunc = new TransferFunction(); opacFunc.addPoint( (float)
             * parentFrame.getImageA().getMin(), 100 ); opacFunc.addPoint( 79, 100 ); opacFunc.addPoint( 655, 0 );
             * opacFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0 );
             *
             * loadVisualizationSettings( lutFunc, opacFunc );*/

            float[] xLut = new float[4];
            float[] yLut = new float[4];

            xLut[0] = (float) parentFrame.getImageA().getMin();
            yLut[0] = 255;

            xLut[1] = -206;
            yLut[1] = 255;

            xLut[2] = 356;
            yLut[2] = 0;

            xLut[3] = (float) parentFrame.getImageA().getMax();
            yLut[3] = 0;

            TransferFunction lutTrans = new TransferFunction();

            lutTrans.importArrays(xLut, yLut, 4);

            float[] xOp = new float[4];
            float[] yOp = new float[4];

            xOp[0] = (float) parentFrame.getImageA().getMin();
            yOp[0] = 100;

            xOp[1] = 79;
            yOp[1] = 100;

            xOp[2] = 655;
            yOp[2] = 0;

            xOp[3] = (float) parentFrame.getImageA().getMax();
            yOp[3] = 0;

            TransferFunction opacTrans = new TransferFunction();

            opacTrans.importArrays(xOp, yOp, 4);

            loadVisualizationSettings(lutTrans, opacTrans);
        } else if (command.equals("BonePreset")) {
            // load bone presets
            /* TransferFunction lutFunc = new TransferFunction();
             * lutFunc.addPoint( (float) parentFrame.getImageA().getMin(), 255 ); lutFunc.addPoint( -206, 255 );
             * lutFunc.addPoint( 356, 0 ); lutFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0 );
             *
             * TransferFunction opacFunc = new TransferFunction(); opacFunc.addPoint( (float)
             * parentFrame.getImageA().getMin(), 100 ); opacFunc.addPoint( 255, 100 ); opacFunc.addPoint( 718, 0 );
             * opacFunc.addPoint( (float) parentFrame.getImageA().getMax(), 0 );
             *
             * loadVisualizationSettings( lutFunc, opacFunc );*/

            float[] xLut = new float[4];
            float[] yLut = new float[4];

            xLut[0] = (float) parentFrame.getImageA().getMin();
            yLut[0] = 255;

            xLut[1] = -206;
            yLut[1] = 255;

            xLut[2] = 356;
            yLut[2] = 0;

            xLut[3] = (float) parentFrame.getImageA().getMax();
            yLut[3] = 0;

            TransferFunction lutTrans = new TransferFunction();

            lutTrans.importArrays(xLut, yLut, 4);

            float[] xOp = new float[4];
            float[] yOp = new float[4];

            xOp[0] = (float) parentFrame.getImageA().getMin();
            yOp[0] = 100;

            xOp[1] = 255;
            yOp[1] = 100;

            xOp[2] = 718;
            yOp[2] = 0;

            xOp[3] = (float) parentFrame.getImageA().getMax();
            yOp[3] = 0;

            TransferFunction opacTrans = new TransferFunction();

            opacTrans.importArrays(xOp, yOp, 4);

            loadVisualizationSettings(lutTrans, opacTrans);
        } else if (command.equals("LoadPreset")) {
            String dirName = parentFrame.getImageA().getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            String custom_preset_filebase = parentFrame.getImageA().getImageName() + "_rfast_visual.fun";

            loadVisualizationDataFrom(false, custom_preset_filebase, dirName, false);
        } else if (command.equals("SavePreset")) {
            String dirName = parentFrame.getImageA().getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            String custom_preset_filebase = parentFrame.getImageA().getImageName() + "_rfast_visual.fun";

            saveVisualizationDataAs(false, custom_preset_filebase, dirName);
        } else if (command.equals("ShowLabels")) {
            showBurnLabels = showBurnLabelsCB.isSelected();
            burnBase.enableBurnLabels(showBurnLabels);
        } else if (command.equals("snapToTarget")) {
            snapToTarget = snapToTargetCB.isSelected();
        } else if (command.equals("loadRegionMap")) {
            ModelImage mapImg = loadImage();

            parentFrame.setSegmentationImage(mapImg);
        } else if (command.equals("saveRegionMap")) {
            ModelImage mapImg = parentFrame.getSegmentationImage();

            if (mapImg == null) {
                MipavUtil.displayError("There is no region map image loaded.");

                return;
            }

            saveImage(mapImg);
        } else if (command.equals("centerOnSurface")) {
            int selectedIndex = targetList.getMinSelectionIndex();

            if (selectedIndex == -1) {
                return;
            }

            Vector<SurfaceAttributes> surfaceVector = (Vector<SurfaceAttributes>) ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().clone();

            for (int i = 0; i < surfaceVector.size(); i++) {
                String name = ((SurfaceAttributes) surfaceVector.get(i)).getName();

                if (name.endsWith("_liver.sur") || name.endsWith("_vasculature.sur")) {
                    surfaceVector.remove(i);
                    i--;
                }
            }

            if (surfaceVector.size() == 0) {
                return;
            }

            Point3f center = ((SurfaceAttributes) surfaceVector.get(selectedIndex)).getCenter();
            Vector3f surfaceCenter = new Vector3f(center.x, center.y, center.z);
            Transform3D t3d = new Transform3D();

            probe.getProbRootParentTG().getTransform(t3d);
            t3d.setTranslation(surfaceCenter);
            probe.getProbRootParentTG().setTransform(t3d);
        }
    }

    /**
     * Detect the body tissue component. If the bone or the vasculature is detected, draw the corresponding color along
     * the probing path, also, show the detection message in the search panel.
     */
    public void detectTissue() {
        int result;
        float startX, startY, startZ;
        float endX, endY, endZ;
        float stepX, stepY, stepZ;
        boolean updateLabel = true;
        Transform3D startPtTrans = new Transform3D();
        Transform3D endPtTrans = new Transform3D();
        Transform3D burnPtTrans = new Transform3D();

        if (!probe.isProbeRootParentBGLive()) {
            return;
        }

        startPtTrans = probe.getStartPointCoordinate();
        endPtTrans = probe.getEndPointCoordinate();
        burnPtTrans = probe.getCoordinate();
        startPtTrans.get(startPtTransVector);
        endPtTrans.get(endPtTransVector);
        burnPtTrans.get(burningPtTransVector);
        startX = startPtTransVector.x;
        startY = startPtTransVector.y;
        startZ = startPtTransVector.z;
        endX = endPtTransVector.x;
        endY = endPtTransVector.y;
        endZ = endPtTransVector.z;
        stepX = (endX - startX) / 300.0f;
        stepY = (endY - startY) / 300.0f;
        stepZ = (endZ - startZ) / 300.0f;

        for (int idx = 0; idx < 300; idx++) {
            float x, y, z;

            x = startX + (idx * stepX);
            y = startY + (idx * stepY);
            z = startZ + (idx * stepZ);
            result = ((SurfaceRender) renderBase).whichTissue(x, y, z);

            if (result != -1) {
                resultLabel.setForeground(Color.black);

                if (result == SurfaceRender.ENTRY_POINT) {
                    probe.updateEntryPointBG(x, y, z);
                } else if (result == SurfaceRender.BONE_SEG) {

                    // System.out.println("find bone haha ");
                    resultLabel.setForeground(Color.red);
                    resultLabel.setText("bone");

                    if (updateLabel) {
                        probe.updateBoneBG(x, y, z);
                    }

                    updateLabel = false;
                } else if (result == SurfaceRender.TUMOR_SEG) {

                    // System.out.println("find Tumor haha " + count);
                    resultLabel.setText("Tumor");
                    updateLabel = false;
                } else if (result == SurfaceRender.VASCULATURE_SEG) {

                    // System.out.println("find Vasculature haha " + count);
                    resultLabel.setForeground(Color.blue);
                    resultLabel.setText("Vasculature");

                    if (updateLabel) {
                        probe.updateVasculatureBG(x, y, z);
                    }

                    updateLabel = false;
                } else if (result == SurfaceRender.ARTERIAL_SEG) {

                    // System.out.println("find Arterial haha " + count);
                    resultLabel.setText("Arterial");
                    updateLabel = false;
                } else if (result == SurfaceRender.VEINOUS_SEG) {

                    // System.out.println("find Veinous haha " + count);
                    resultLabel.setText("Veinous");
                    updateLabel = false;
                }

                // draw the sphere voxels kernel, the idea is to draw the voxels only when a bone, or a vasculature
                // detected along the probing path.   This approach saves the time to draw the unnecessary voxels on the
                // path, otherwise, the performance is too low.
                /* if ( probe.isProbeRootParentBGLive() ) {
                 * x = startX + idx * stepX; y = startY + idx * stepY; z = startZ + idx * stepZ;
                 *
                 * xMin = (x - r); yMin = (y - r); zMin = (z - r);
                 *
                 * xMax = (x + r); yMax = (y + r); zMax = (z + r);
                 *
                 * for (i = xMin; i < xMax; i = i + stepSize) { for (j = yMin; j < yMax; j = j + stepSize) { for (k =
                 * zMin; k < zMax; k = k + stepSize) { // if (withinDistance(x, y, z, i, j, k, r)) { ( (SurfaceRender)
                 * renderBase).drawProbingVoxel(i, j, k, result); // } } } } } */
            }
        }

        if (updateLabel) {
            resultLabel.setForeground(Color.black);
            resultLabel.setText("No intersection with important structures");
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {

        if (probe != null) { // probe.dispose();
        }

        if (burnBase != null) {
            burnBase.dispose();
        }
    }

    /**
     * Enable probe mouse behavior branch group.
     *
     * @param  flag  <code>true</code> means turn on, <code>false</code> means turn off.
     */
    public void enableProbeBehavior(boolean flag) {
        probe.enableProbeBehavior(flag);
    }

    /**
     * Get the burn base reference inside the probe control panel.
     *
     * @return  BurnBase
     */
    public BurnBase getBurnBase() {
        return burnBase;
    }

    /**
     * Get the show burning point label's flag.
     *
     * @return  boolean show label or not.
     */
    public boolean getBurnLabelFlag() {
        return showBurnLabels;
    }

    /**
     * Get the burning point world coordinate.
     *
     * @return  Vector3f burning point tranform vector.
     */
    public Vector3f getBurnPoint() {
        return burningPtTransVector;
    }

    /**
     * Get the burn root branch group. JPanelSurface calls this method during the geometry picking procedure.
     *
     * @return  BranchGroup
     */
    public BranchGroup getBurnRootParentBG() {
        return burnBase.getBurnRootParentBG();
    }

    /**
     * Get the current highlight surface.
     *
     * @return  SurfaceAttributes
     */
    public SurfaceAttributes getCurSurface() {
        int index = targetList.getSelectedIndex();
        Vector<SurfaceAttributes> surfaceVector = (Vector<SurfaceAttributes>) ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().clone();

        for (int i = 0; i < surfaceVector.size(); i++) {
            String name = ((SurfaceAttributes) surfaceVector.get(i)).getName();

            if (name.endsWith("_liver.sur") || name.endsWith("_vasculature.sur")) {
                surfaceVector.remove(i);
                i--;
            }
        }

        return (SurfaceAttributes) (surfaceVector.elementAt(index));
    }

    /**
     * Get the treatment info that matches the currently selected target surface.
     *
     * @return  info about a treatment
     */
    public TreatmentInformation getCurTreatment() {
        int index = targetList.getSelectedIndex();

        if (index == -1) {
            return noTargetTreatment;
        } else {
            return getTreatmentInfo(targetList.getSelectedIndex());
        }
    }

    /**
     * Get the end point world coordinate vector.
     *
     * @return  Vector3f end point transform vector.
     */
    public Vector3f getEndPoint() {
        return endPtTransVector;
    }

    /**
     * Get the entry ponit world coordinate.
     *
     * @return  Point3f entry ponit transform vector.
     */
    public Point3f getEntryPoint() {
        return probe.getEntryPoint();
    }

    /**
     * Get the main control panel.
     *
     * @return  JPanel main control panel.
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Get the probe base reference inside the burning control panel.
     *
     * @return  Probe
     */
    public Probe getProbeBase() {
        return probe;
    }

    /**
     * Return the root to the probes branches.
     *
     * @return  BranchGroup probes branch group
     */
    public BranchGroup getProbesRootBG() {
        return probe.getProbesRootBG();
    }

    /**
     * Get the start point world coordinate vector.
     *
     * @return  Vector3f start point tranform vector.
     */
    public Vector3f getStartPoint() {
        return startPtTransVector;
    }

    /**
     * Return the currently targeted surface.
     *
     * @return  the attributes of the current target surface
     */
    public SurfaceAttributes getTargetSurface() {
        Vector<SurfaceAttributes> surfaceVector = (Vector<SurfaceAttributes>) ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().clone();

        for (int i = 0; i < surfaceVector.size(); i++) {
            String name = ((SurfaceAttributes) surfaceVector.get(i)).getName();

            if (name.endsWith("_liver.sur") || name.endsWith("_vasculature.sur")) {
                surfaceVector.remove(i);
                i--;
            }
        }

        if (surfaceVector.size() == 0) {
            return null;
        }

        return (SurfaceAttributes) surfaceVector.get(targetList.getSelectedIndex());
    }

    /**
     * Return whether the user wants to snap to the treatment target surface.
     *
     * @return  whether to snap to the target
     */
    public boolean isSnapToTarget() {
        return snapToTarget;
    }

    /**
     * This method loads the LUT and opacity transfer function for the active image.
     *
     * @param  loadAll    boolean indicating that both lut and transfer functions should be loaded. If false, then only
     *                    transfer functions are loaded.
     * @param  filename   filename to load LUT from
     * @param  dirName    directory to load LUT from
     * @param  quietMode  if true indicates that warnings should not be displayed.
     */
    public void loadVisualizationDataFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;

        if (parentFrame.getDisplayMode() == ViewJFrameBase.IMAGE_A) {
            img = parentFrame.getImageA();
            lut = parentFrame.getLUTa();
        } else {
            img = parentFrame.getImageB();
            lut = parentFrame.getLUTb();
        }

        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (loadAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }
        }

        try {
            fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

            TransferFunction opacFunction = new TransferFunction();

            fileHistoLUT.setOpacityFunction(opacFunction);
            fileHistoLUT.writeLUT(lut);

            if (loadAll) {
                fileHistoLUT.readLUTandTransferFunction(quietMode);
            } else {
                fileHistoLUT.readFunctions();
            }

            if (parentFrame.getDisplayMode() == ViewJFrameBase.IMAGE_A) {
                parentFrame.setLUTa(lut);
            } else {
                parentFrame.setLUTb(lut);
            }

            loadVisualizationSettings(lut.getTransferFunction(), opacFunction);
        } catch (IOException error) {

            if (!quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    }

    /**
     * During mouse release, remove the probing path and clear the probe color mask.
     */
    public void removeProbingPath() {
        ((SurfaceRender) renderBase).enableEntryPoint(false);
        probe.removeIndicator();
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   the panel should be
     * @param  frameHeight  the height of the renderer frame
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.setSize(new Dimension(panelWidth, frameHeight - (toolBar.getHeight() * 2)));
        scroller.revalidate();

    }

    /**
     * This method saves the LUT and opacity function for the active image. If either filename or directory is null,
     * then the user will be prompted for a filename.
     *
     * @param  saveAll   boolean indicating that both lut and transfer functions should be saved. If false, then only
     *                   transfer functions are saved.
     * @param  filename  filename to save LUT as
     * @param  dirName   directory to save LUT to
     */
    public void saveVisualizationDataAs(boolean saveAll, String filename, String dirName) {
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;

        if (parentFrame.getDisplayMode() == ViewJFrameBase.IMAGE_A) {
            img = parentFrame.getImageA();
            lut = parentFrame.getLUTa();
        } else {
            img = parentFrame.getImageB();
            lut = parentFrame.getLUTb();
        }

        // if filename and/or dirName is null, then get it from user
        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (saveAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }

        }

        try {
            fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

            // VOIBase opacFunction = ( (SurfaceRender) renderBase ).getVolOpacityPanel().
            // getSelectedComponent( JPanelVolOpacity.COMP_A ).getTransferFunction();
            // fileHistoLUT.setOpacityFunction( opacFunction );

            if (saveAll) {
                fileHistoLUT.writeAll();
            } else {
                fileHistoLUT.writeFunctions();
            }

        } catch (IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }
    }

    /**
     * When the probe is picked, switch the probe geometry appearance to green.
     *
     * @param  flag  boolean probe picked or not.
     */
    public void setProbeGreenColor(boolean flag) {
        probe.setProbeGreenColor(flag);
    }

    /**
     * When the burn button is clicked, invoked the start burn method to show the burnning process.
     */
    public void startBurn() {
        Vector3f translate;
        Point3f entryPoint;

        burnIndex++;

        enableBurnVisComponents(true);
        enableBurnTreatmentComponents(true);

        String name = "burn" + burnIndex;

        opacity = 1.0f;

        // placeholder in case we need to incorporate burning time in the future
        float burningTime = 2.0f;

        burnBase.startBurn(((Float) tipLengthCB.getSelectedItem()).floatValue(), burningTime);

        translate = burnBase.getTranslate();
        center = new Point3f(translate.x, translate.y, translate.z);
        entryPoint = probe.getEntryPoint();

        boolean pickable = false;

        clipping = burnClipCB.isSelected();
        culling = burnBackFaceCB.isSelected();
        // showBurning = showBurningCB.isSelected();

        burnBackFaceCB.setSelected(culling);
        burnClipCB.setSelected(clipping);

        // showBurningCB.setSelected( showBurning );
        colorButton.setBackground(burnBase.getColor().get());

        // / should these be in voxel space instead of real (eg. mm) space?
        Point3f volPt = transformPtVolumeToImage(center);

        xText.setText(" " + MipavUtil.makeFloatString(volPt.x * xRes, 3));
        yText.setText(" " + MipavUtil.makeFloatString(volPt.y * yRes, 3));
        zText.setText(" " + MipavUtil.makeFloatString(volPt.z * zRes, 3));

        Point3f entryPt = transformPtVolumeToImage(entryPoint);

        xTextEntry.setText(MipavUtil.makeFloatString(entryPt.x * xRes, 3));
        yTextEntry.setText(MipavUtil.makeFloatString(entryPt.y * yRes, 3));
        zTextEntry.setText(MipavUtil.makeFloatString(entryPt.z * zRes, 3));

        volumeText.setText(" " + MipavUtil.makeFloatString(burnBase.getVolume(), 3));

        Point3f radius = burnBase.getRadius();

        // ruida
        semiXText.setText(" " + MipavUtil.makeFloatString(radius.x, 3));
        semiYText.setText(" " + MipavUtil.makeFloatString(radius.y, 3));
        semiZText.setText(" " + MipavUtil.makeFloatString(radius.z, 3));

        BurnAttributes burn = new BurnAttributes(burnBase.getSphereBG(), name, burnBase.getColor(),
                                                 burnBase.getVolume(), opacity, radius, burningTime, center, pickable,
                                                 clipping, culling, burnBase.getMask(), getEntryPoint(), getBurnPoint(),
                                                 probe.getTransform());

        Vector<String> burnNames = new Vector<String>();

        getCurTreatment().addBurn(burn);

        for (Enumeration<BurnAttributes> en = getCurTreatment().getBurnEnum(); en.hasMoreElements();) {
            burnNames.addElement(((BurnAttributes) en.nextElement()).name);
        }

        burnList.setListData(burnNames);
        burnList.setSelectedIndex(burnNames.size() - 1);

        // only update of burn info, don't swap of all panel info..
        updateTreatmentPanel(false);
    }

    /**
     * State change event, invokded by the sliders moves.
     *
     * @param  e  ChangeEvent
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == sliderX) {

            // Change the currently displayed x slice
            float xSlice = sliderX.getValue() / 100.0f;

            probe.setProbeCoordinate(xSlice);

            if (!sliderX.getValueIsAdjusting()) {
                detectTissue();
            } else {
                removeProbingPath();
            }
        } else if (source == opacitySlider) {

            // operates on all burns attached to the current target surface treatment
            for (int i = 0; i < getCurTreatment().getNumBurns(); i++) {
                int iValue = opacitySlider.getValue();

                getCurTreatment().getBurn(i).opacity = 1 - (iValue / 100.0f);

                // change the object opacity
                BranchGroup root = getCurTreatment().getBurn(i).burnBG;
                Shape3D shape = burnBase.getShape(root);

                Appearance appearance = shape.getAppearance();
                TransparencyAttributes tap = new TransparencyAttributes();

                tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

                if ((1 - (iValue / 100.0f)) == 0) {
                    tap.setTransparencyMode(TransparencyAttributes.NONE);
                } else {
                    tap.setTransparencyMode(TransparencyAttributes.BLENDED);
                }

                tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
                tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
                tap.setTransparency(1 - (iValue / 100.0f)); // 0 = Opaque

                // appearance.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);
                appearance.setTransparencyAttributes(tap);
            }
        } else if (source == xSliderS) {
            Point3f kPosition = new Point3f();

            kPosition.x = (float) (xSliderS.getValue() / 100.0);
            kPosition.y = (float) (ySliderS.getValue() / 100.0);
            kPosition.z = (float) (zSliderS.getValue() / 100.0);
            probe.updatePosition(kPosition);

            if (!xSliderS.getValueIsAdjusting()) {
                detectTissue();
            } else {
                removeProbingPath();
            }

        } else if (source == ySliderS) {
            Point3f kPosition = new Point3f();

            kPosition.x = (float) (xSliderS.getValue() / 100.0);
            kPosition.y = (float) (ySliderS.getValue() / 100.0);
            kPosition.z = (float) (zSliderS.getValue() / 100.0);
            probe.updatePosition(kPosition);

            if (!ySliderS.getValueIsAdjusting()) {
                detectTissue();
            } else {
                removeProbingPath();
            }

        } else if (source == zSliderS) {
            Point3f kPosition = new Point3f();

            kPosition.x = (float) (xSliderS.getValue() / 100.0);
            kPosition.y = (float) (ySliderS.getValue() / 100.0);
            kPosition.z = (float) (zSliderS.getValue() / 100.0);
            probe.updatePosition(kPosition);

            if (!zSliderS.getValueIsAdjusting()) {
                detectTissue();
            } else {
                removeProbingPath();
            }

        } else if (source == m_kProbeTwistSlider) {
            float fValue = m_kProbeTwistSlider.getValue();

            probe.twist(fValue, m_kProbeTwistSlider.getValueIsAdjusting());
            m_kProbeTwistLabel.setText("Rotation along Probe direction: " + fValue + " Degrees");
        }

    }

    /**
     * Transform a java3d point [-1,1] to the image volume space.
     *
     * @param   pt  the point in java3d space
     *
     * @return  the equivalent point in volume space
     */
    public Point3f transformPtVolumeToImage(Point3f pt) {
        float m_fX0 = -xBox;
        float m_fY0 = -yBox;
        float m_fX1 = xBox;
        float m_fY1 = yBox;

        float m_fZ0 = -zBox;
        float m_fZ1 = zBox;

        if (zBox > maxBox) {
            m_fZ0 = -1f;
            m_fZ1 = 1f;
        }

        int xDim = parentFrame.getImageA().getExtents()[0];
        int yDim = parentFrame.getImageA().getExtents()[1];
        int zDim = parentFrame.getImageA().getExtents()[2];

        Point3f volPt = new Point3f();

        volPt.x = ((pt.x - m_fX0) / (m_fX1 - m_fX0)) * (xDim - 1);
        volPt.y = ((pt.y - m_fY0) / (m_fY1 - m_fY0)) * (yDim - 1);
        volPt.z = ((pt.z - m_fZ0) / (m_fZ1 - m_fZ0)) * (zDim - 1);

        // flip the z and y coordinate
        volPt.z = zDim - 1 - pt.z;
        volPt.y = yDim - 1 - pt.y;

        return volPt;
    }

    /**
     * Updates the burn name list's selected element. Called from the JPanelSurface.
     *
     * @param  index  selected index number.
     */
    public void updateBurnList(int index) {
        burnList.setSelectedIndex(index);
    }

    /**
     * Update probe list.
     */
    public void updateProbeList() {
        Vector<String> probeNames = new Vector<String>();

        probeNames.addElement("default probe");
        probeNames.addElement("thermal probe");
        probeNames.addElement("regular probe");
        probeNames.addElement("CoolTip probe");
        probeList.setListData(probeNames);
        probeList.setSelectedIndex(3);
    }

    /**
     * Updates the probe world coordinate from the tri-planar view's mouse drag event.
     *
     * @param  x  float x coordinate
     * @param  y  float y coordinate
     * @param  z  float z coordinate
     */
    public void updateProbePos(float x, float y, float z) {

        if (cursorMode == PROBEMOVE) {
            probe.updatePosition(new Point3f(x, y, z));
        }
    }

    /**
     * Update target surface list from the JPanelSurface control dialog.
     */
    public void updateTargetList() {
        Vector<String> surfaceNames = new Vector<String>();
        Vector<SurfaceAttributes> surfaceVector = ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector();

        if (treatmentVector != null) {
            treatmentVector.removeAllElements();
        }

        for (Enumeration<SurfaceAttributes> en = surfaceVector.elements(); en.hasMoreElements();) {
            String name = ((SurfaceAttributes) en.nextElement()).getName();

            if (!name.endsWith("_liver.sur") && !name.endsWith("_vasculature.sur")) {
                surfaceNames.addElement(name);
                treatmentVector.add(new TreatmentInformation());
            }
        }

        targetList.setListData(surfaceNames);

        if (targetList.getSelectedIndex() == -1) {

            // disable the panel if no entry selected
            enableBurnTreatmentComponents(false);
        }

        updateTargetLabels();

        if (surfaceNames.size() > 0) {
            targetList.setSelectedIndex(surfaceNames.size() - 1);

            // attach the probe to the surface center if the user wants to
            if (snapToTarget) {
                Point3f center = ((SurfaceAttributes) surfaceVector.get(surfaceVector.size() - 1)).getCenter();
                Vector3f surfaceCenter = new Vector3f(center.x, center.y, center.z);
                Transform3D t3d = new Transform3D();

                probe.getProbRootParentTG().getTransform(t3d);
                t3d.setTranslation(surfaceCenter);
                probe.getProbRootParentTG().setTransform(t3d);
            }
        }
    }

    /**
     * Update the treatment panel components to correpsond to the currently selected target surface.
     *
     * @param  swapBurnList  whether to swap in the burn list of the currently selected target surface treatment (used
     *                       when a new selection is made in the target list)
     */
    public void updateTreatmentPanel(boolean swapBurnList) {

        if (swapBurnList) {
            burnList.removeAll();

            Vector<String> burnNames = new Vector<String>();

            for (Enumeration<BurnAttributes> e = getCurTreatment().getBurnEnum(); e.hasMoreElements();) {
                burnNames.add(((BurnAttributes) e.nextElement()).name);
            }

            burnList.setListData(burnNames);

            if (burnNames.size() > 0) {
                burnList.setSelectedIndex(burnNames.size() - 1);
            }
        }

        // update total volume field
        float vol = getCurTreatment().getTotalVolume();

        if (vol != -1) {
            totalVolumeText.setText(MipavUtil.makeFloatString(vol, 3));
        } else {
            totalVolumeText.setText("");
        }

        float diff = getCurTreatment().getDiffVolume();

        if (diff != -1) {
            diffVolumeText.setText(MipavUtil.makeFloatString(diff, 3));
        } else {
            diffVolumeText.setText("");
        }

        int burnIndex = burnList.getSelectedIndex();

        if (burnIndex != -1) {

            // update center coord from selected burn
            // / should these be in voxel space instead of real (eg. mm) space?
            Point3f volPt = transformPtVolumeToImage(getCurTreatment().getBurn(burnIndex).center);

            xText.setText(MipavUtil.makeFloatString(volPt.x * xRes, 3));
            yText.setText(MipavUtil.makeFloatString(volPt.y * yRes, 3));
            zText.setText(MipavUtil.makeFloatString(volPt.z * zRes, 3));

            Point3f entryPt = transformPtVolumeToImage(getCurTreatment().getBurn(burnIndex).entryPoint);

            xTextEntry.setText(MipavUtil.makeFloatString(entryPt.x * xRes, 3));
            yTextEntry.setText(MipavUtil.makeFloatString(entryPt.y * yRes, 3));
            zTextEntry.setText(MipavUtil.makeFloatString(entryPt.z * zRes, 3));

            // update burn diameter
            Point3f radius = getCurTreatment().getBurnRadius(burnIndex);

            semiXText.setText(" " + MipavUtil.makeFloatString(radius.x, 3));
            semiYText.setText(" " + MipavUtil.makeFloatString(radius.y, 3));
            semiZText.setText(" " + MipavUtil.makeFloatString(radius.z, 3));

            // update burn volume
            // / assumes that the units are the same in all dims
            String unitStr = Unit.getUnitFromLegacyNum(parentFrame.getImageOriginal().getFileInfo(0).getUnitsOfMeasure(0)).getAbbrev();

            volumeText.setText(" " + MipavUtil.makeFloatString(getCurTreatment().getBurnVolume(burnIndex), 3) + " " +
                               unitStr + "^3");

            enableBurnTreatmentComponents(true);
        } else {
            xText.setText("");
            yText.setText("");
            zText.setText("");
            semiXText.setText("");
            semiYText.setText("");
            semiZText.setText("");
            volumeText.setText("");
            enableBurnTreatmentComponents(false);
        }
    }

    /**
     * Value changed event, which invoked by probe name list, burn name list, surface name list, burn param name list.
     *
     * @param  kEvent  ListSelection Event
     */
    public void valueChanged(ListSelectionEvent kEvent) {
        JList kList = (JList) kEvent.getSource();
        int selectedIndex = kList.getMinSelectionIndex();
        int[] indices = kList.getSelectedIndices();

        if (selectedIndex == -1) {
            return;
        }

        if (kList == probeList) {
            probe.activeProbe(selectedIndex);

            if (entryPointRotationCB.isSelected()) {
                probe.enableEntryPointRotation(true);
            }

            burnBase.activeBurn(selectedIndex);

            float xSlice = sliderX.getValue() / 100.0f;

            probe.setProbeCoordinate(xSlice);

            if (probe.getProbeType() != Probe.COOLTIP_PROBE) {
                tipLengthCB.setEnabled(false);
            } else {
                tipLengthCB.setEnabled(true);
            }
        } else if ((kList == burnList) && (getCurTreatment().getNumBurns() > 0)) {
            BurnAttributes attributes;
            BranchGroup root = getCurTreatment().getBurn(selectedIndex).burnBG;

            burnBackFaceCB.setSelected(root.getCollidable());

            if (((SurfaceRender) renderBase).getDisplayMode3D()) {
                burnClipCB.setSelected(root.getAlternateCollisionTarget());
            }

            // showBurningCB.setSelected( ( (BurnAttributes) burnVector.get( selectedIndex ) ).showBurning );
            attributes = getCurTreatment().getBurn(selectedIndex);

            Color4f color = attributes.color;

            colorButton.setBackground(color.get());

            // / should these be in voxel space instead of real (eg. mm) space?
            Point3f volPt = transformPtVolumeToImage(attributes.center);

            xText.setText(" " + MipavUtil.makeFloatString(volPt.x * xRes, 3));
            yText.setText(" " + MipavUtil.makeFloatString(volPt.y * yRes, 3));
            zText.setText(" " + MipavUtil.makeFloatString(volPt.z * zRes, 3));

            // set the probe position for the current burn treatment.
            probe.getProbRootParentTG().setTransform(attributes.transform);

            Point3f entryPt = transformPtVolumeToImage(attributes.entryPoint);

            xTextEntry.setText(" " + MipavUtil.makeFloatString(entryPt.x * xRes, 3));
            yTextEntry.setText(" " + MipavUtil.makeFloatString(entryPt.y * yRes, 3));
            zTextEntry.setText(" " + MipavUtil.makeFloatString(entryPt.z * zRes, 3));

            // / assumes that the units are the same in all dims
            String unitStr = Unit.getUnitFromLegacyNum(parentFrame.getImageOriginal().getFileInfo(0).getUnitsOfMeasure(0)).getAbbrev();

            volumeText.setText(" " + MipavUtil.makeFloatString(attributes.volume, 3) + " " + unitStr + "^3");

            Point3f radius = attributes.radius;

            semiXText.setText(" " + MipavUtil.makeFloatString(radius.x, 3));
            semiYText.setText(" " + MipavUtil.makeFloatString(radius.y, 3));
            semiZText.setText(" " + MipavUtil.makeFloatString(radius.z, 3));

            opacitySlider.setValue((int) (attributes.opacity * 100));
            enableBurnTreatmentComponents(true);
        } else if ((kList == targetList) &&
                       (((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().size() > 0)) {
            Vector<SurfaceAttributes> surfaceVector = (Vector<SurfaceAttributes>) ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().clone();

            for (int i = 0; i < surfaceVector.size(); i++) {
                String name = ((SurfaceAttributes) surfaceVector.get(i)).getName();

                if (name.endsWith("_liver.sur") || name.endsWith("_vasculature.sur")) {
                    surfaceVector.remove(i);
                    i--;
                }
            }

            if (surfaceVector.size() == 0) {
                return;
            }

            if (indices.length == 1) {
                Vector<SurfaceAttributes> surfaces = ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector();

                for (int i = 0; i < surfaces.size(); i++) {

                    if (((SurfaceAttributes) surfaces.get(i)).getName().equals(((SurfaceAttributes)
                                                                                    surfaceVector.get(selectedIndex))
                                                                                   .getName())) {
                        ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceList().setSelectedIndex(i);
                    }
                }
            } else if (indices.length > 1) {
                Vector<SurfaceAttributes> surfaces = ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector();

                for (int i = 0; i < surfaces.size(); i++) {

                    if (((SurfaceAttributes) surfaces.get(i)).getName().equals(((SurfaceAttributes)
                                                                                    surfaceVector.get(selectedIndex))
                                                                                   .getName())) {
                        ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceList().setSelectedIndex(i);
                    }
                }

                MipavUtil.displayError("You shouldn't select more than one target surface.");
            }

            if (snapToTarget) {
                Point3f center = ((SurfaceAttributes) surfaceVector.get(selectedIndex)).getCenter();
                Vector3f surfaceCenter = new Vector3f(center.x, center.y, center.z);
                Transform3D t3d = new Transform3D();

                probe.getProbRootParentTG().getTransform(t3d);
                t3d.setTranslation(surfaceCenter);
                probe.getProbRootParentTG().setTransform(t3d);
            }

            updateTargetLabels();

            updateTreatmentPanel(true);
        }
    }

    /**
     * Check to see if the given voxel is within the sphere radius from the center point.
     *
     * @param   centerX  float center x coordinate
     * @param   centerY  float center y coordinate
     * @param   centerZ  float center z coordinate
     * @param   ptrX     float given point x coordinate
     * @param   ptrY     float given point x coordinate
     * @param   ptrZ     float given point x coordinate
     * @param   r        double given radius
     *
     * @return  boolean true within the distance, false not.
     */
    public boolean withinDistance(float centerX, float centerY, float centerZ, float ptrX, float ptrY, float ptrZ,
                                  double r) {

        double distance = Math.sqrt(((ptrX - centerX) * (ptrX - centerX)) + ((ptrY - centerY) * (ptrY - centerY)) +
                                    ((ptrZ - centerZ) * (ptrZ - centerZ)));

        if (distance <= r) {
            return true;
        } else {
            return false;
        }

    }

    /**
     * Creates a label in the proper (bold) font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    private static JLabel createBoldLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(MipavUtil.font12B);
        label.setForeground(Color.black);

        return label;
    }

    /**
     * Creates a label in the proper font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    private static JLabel createLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
    }

    /**
     * Build the tab panel containing ablation (sphere/ellipsoid) packing parameters, etc.
     */
    private void buildBurnPackingPanel() {
        JPanel packingPanel = new JPanel(new GridBagLayout());

        packingPanel.setBorder(MipavUtil.buildTitledBorder("Burn packing parameters"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;

        JButton packingButton = new JButton("Start burn packing");

        packingButton.setActionCommand("burnPacking");
        packingButton.setFont(MipavUtil.font12B);
        packingButton.addActionListener(this);
        packingPanel.add(packingButton, gbc);

        tabbedPane.addTab("Burn Packing", null, packingPanel);
    }

    /**
     * Build the tab containing the color, opac, picking, labels, etc for burns.
     */
    private void buildBurnVisPanel() {
        JPanel burnPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;

        // Burn option panel
        JPanel burnOptionPanel = new JPanel(new GridBagLayout());

        burnOptionPanel.setBorder(MipavUtil.buildTitledBorder("Burn visualization options"));

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

        // Slider for changing opacity; not enabled initially.
        opacityLabel = new JLabel("Opacity");
        opacityLabel.setFont(MipavUtil.font12B);
        opacityLabel.setForeground(Color.black);

        opacitySliderLabels = new JLabel[3];
        opacitySliderLabels[0] = new JLabel("0");
        opacitySliderLabels[1] = new JLabel("50");
        opacitySliderLabels[2] = new JLabel("100");

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

        JPanel opacitySliderPanel = new JPanel();

        opacitySliderPanel.setLayout(new BoxLayout(opacitySliderPanel, BoxLayout.Y_AXIS));
        opacitySliderPanel.add(opacityLabel);
        opacitySliderPanel.add(opacitySlider);

        burnClipCB = new JCheckBox("Burn Clipping", false);
        burnClipCB.setToolTipText("Burning point surface clipping.");
        burnClipCB.addActionListener(this);
        burnClipCB.setActionCommand("Clipping");
        burnClipCB.setFont(MipavUtil.font12B);
        burnClipCB.setEnabled(false);

        burnBackFaceCB = new JCheckBox("Backface Culling", true);
        burnBackFaceCB.setToolTipText("Burning point surface back face culling.");
        burnBackFaceCB.addActionListener(this);
        burnBackFaceCB.setActionCommand("backface");
        burnBackFaceCB.setFont(MipavUtil.font12B);

        JPanel cbBurnPanel = new JPanel();

        cbBurnPanel.setLayout(new BoxLayout(cbBurnPanel, BoxLayout.Y_AXIS));
        cbBurnPanel.add(burnClipCB);
        cbBurnPanel.add(burnBackFaceCB);

        JPanel mixPanel = new JPanel();

        mixPanel.setLayout(new BoxLayout(mixPanel, BoxLayout.X_AXIS));
        mixPanel.add(cbBurnPanel);

        gbc.gridx = 0;
        gbc.gridy = 0;
        burnOptionPanel.add(colorPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        burnOptionPanel.add(opacitySliderPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        burnOptionPanel.add(mixPanel, gbc);

        JPanel globalBurnOptionPanel = new JPanel(new GridLayout());

        globalBurnOptionPanel.setBorder(MipavUtil.buildTitledBorder("Global Burn Options"));
        showBurnLabelsCB = new JCheckBox("Show burn labels", showBurnLabels);
        showBurnLabelsCB.setActionCommand("ShowLabels");
        showBurnLabelsCB.setFont(MipavUtil.font12B);
        showBurnLabelsCB.addActionListener(this);
        globalBurnOptionPanel.add(showBurnLabelsCB);

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;
        burnPanel.add(burnOptionPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy++;
        burnPanel.add(globalBurnOptionPanel, gbc);

        tabbedPane.addTab("Burn", null, burnPanel);
    }

    /**
     * Build the tab which allows the user to change the currently used probe model.
     */
    private void buildProbeModelPanel() {

        // Probe panel
        probeList = new JList();
        probeList.addListSelectionListener(this);

        JScrollPane probeScrollPane = new JScrollPane(probeList);
        JPanel probeScrollPanel = new JPanel();

        probeScrollPanel.setLayout(new BorderLayout());
        probeScrollPanel.add(probeScrollPane);
        probeScrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel probeListPanel = new JPanel(new BorderLayout());

        probeListPanel.add(probeScrollPanel, BorderLayout.CENTER);
        probeListPanel.setBorder(MipavUtil.buildTitledBorder("Probe list"));

        JLabel tipLengthLabel = new JLabel("Tip length (cm):");

        tipLengthLabel.setFont(MipavUtil.font12B);
        tipLengthLabel.setBackground(Color.black);

        tipLengthCB = new JComboBox();
        tipLengthCB.addItem(new Float(2.0));
        tipLengthCB.addItem(new Float(2.5));
        tipLengthCB.addItem(new Float(3.0));
        tipLengthCB.addItem(new Float(5.0));
        tipLengthCB.addItem(new Float(7.0));
        tipLengthCB.setFont(MipavUtil.font12);
        tipLengthCB.setForeground(Color.black);

        // default to 3.0 cm tip
        tipLengthCB.setSelectedIndex(2);

        if (probeList.getSelectedIndex() != Probe.COOLTIP_PROBE) {
            tipLengthCB.setEnabled(false);
        }

        JPanel probeParamPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;
        probeParamPanel.add(tipLengthLabel, gbc);
        gbc.gridx++;
        probeParamPanel.add(tipLengthCB, gbc);
        probeParamPanel.setBorder(MipavUtil.buildTitledBorder("Probe parameters"));

        JPanel probePanel = new JPanel();

        probePanel.setLayout(new BoxLayout(probePanel, BoxLayout.Y_AXIS));
        probePanel.add(probeListPanel);
        probePanel.add(probeParamPanel);
        tabbedPane.addTab("Probe", null, probePanel);
    }

    /**
     * Build the tab with controls / info used in probe placement before a burn. This includes rot, trans, in/out, entry
     * pt rotation, warning of intersection of problem regions, and the saving / loading of a problem region mapping.
     */
    private void buildProbePlacementPanel() {

        // detection Panel
        JPanel searchPanel = new JPanel();

        searchPanel.setLayout(new GridBagLayout());
        searchPanel.setBorder(MipavUtil.buildTitledBorder("Detection"));

        JLabel searchLabel = new JLabel("Detecting : ");

        searchLabel.setFont(MipavUtil.font12B);
        searchLabel.setForeground(Color.black);

        resultLabel = new JLabel("");
        resultLabel.setFont(MipavUtil.font12B);
        resultLabel.setForeground(Color.black);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        searchPanel.add(searchLabel, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        searchPanel.add(resultLabel, gbc);

        // save/load of problem region mapping
        JPanel mapPanel = new JPanel(new GridBagLayout());

        mapPanel.setBorder(MipavUtil.buildTitledBorder("Region map"));

        JButton loadMapButton = new JButton("Load region map");

        loadMapButton.addActionListener(this);
        loadMapButton.setActionCommand("loadRegionMap");
        loadMapButton.setFont(MipavUtil.font12B);
        loadMapButton.setMinimumSize(MipavUtil.defaultButtonSize);

        JButton saveMapButton = new JButton("Save region map");

        saveMapButton.addActionListener(this);
        saveMapButton.setActionCommand("saveRegionMap");
        saveMapButton.setFont(MipavUtil.font12B);
        saveMapButton.setMinimumSize(MipavUtil.defaultButtonSize);

        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        mapPanel.add(loadMapButton, gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx++;
        mapPanel.add(saveMapButton, gbc);

        // entry point panel
        entryPointRotationCB = new JCheckBox("Entry Point Rotation", false);
        entryPointRotationCB.addActionListener(this);
        entryPointRotationCB.setActionCommand("entryPointRotation");
        entryPointRotationCB.setFont(MipavUtil.font12B);

        GridBagConstraints kGBC = new GridBagConstraints();

        kGBC.anchor = GridBagConstraints.WEST;
        kGBC.gridx = 0;
        kGBC.gridy = 0;

        entryPointPanel = new JPanel();
        entryPointPanel.setLayout(new GridBagLayout());
        entryPointPanel.setBorder(MipavUtil.buildTitledBorder("Rotation"));
        entryPointPanel.add(entryPointRotationCB, kGBC);

        kGBC.gridy++;
        m_kProbeTwistLabel = new JLabel("Rotation along Probe direction: 0 Degrees");
        entryPointPanel.add(m_kProbeTwistLabel, kGBC);

        kGBC.gridy++;
        m_kProbeTwistSlider = new JSlider(-180, 180, 0);

        JLabel[] degreeLabels = new JLabel[3];

        degreeLabels[0] = createLabel("-180");
        degreeLabels[1] = createLabel("0");
        degreeLabels[2] = createLabel("180");

        Hashtable<Integer,JLabel> labels = new Hashtable<Integer,JLabel>();

        labels.put(new Integer(-180), degreeLabels[0]);
        labels.put(new Integer(0), degreeLabels[1]);
        labels.put(new Integer(180), degreeLabels[2]);

        m_kProbeTwistSlider.setFont(MipavUtil.font12);
        m_kProbeTwistSlider.setMajorTickSpacing(15);
        m_kProbeTwistSlider.setPaintTicks(true);
        m_kProbeTwistSlider.setPaintLabels(true);
        m_kProbeTwistSlider.addChangeListener(this);
        m_kProbeTwistSlider.setLabelTable(labels);
        m_kProbeTwistSlider.setAlignmentX(Component.LEFT_ALIGNMENT);

        kGBC.anchor = GridBagConstraints.NORTH;
        kGBC.fill = GridBagConstraints.BOTH;
        kGBC.weightx = 1;
        kGBC.weighty = 1;

        entryPointPanel.add(m_kProbeTwistSlider, kGBC);

        JPanel placementPanel = new JPanel(new GridBagLayout());

        gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        placementPanel.add(buildProbeSlider(), gbc);
        gbc.gridy++;
        placementPanel.add(searchPanel, gbc);
        gbc.gridy++;
        placementPanel.add(mapPanel, gbc);
        gbc.gridy++;
        placementPanel.add(entryPointPanel, gbc);
        tabbedPane.addTab("Placement", null, placementPanel);
    }

    /**
     * Setup the probe moving control panel that include x, y, z moving sliders.
     *
     * @return  the trans x, y, z and in/out probe movement panel.
     */
    private JPanel buildProbeSlider() {
        JPanel sliderPanel = new JPanel(new GridBagLayout());

        sliderPanel.setBorder(MipavUtil.buildTitledBorder(""));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 100;
        gbc.weighty = 100;

        gbc.gridx = 0;
        gbc.gridy = 0;

        JLabel labelX = new JLabel("Move  ");

        labelX.setForeground(Color.black);
        labelX.setFont(MipavUtil.font12);
        labelX.setEnabled(true);
        sliderPanel.add(labelX, gbc);

        sliderX = new JSlider(0, 100, 0);
        sliderX.setFont(MipavUtil.font12);
        sliderX.setEnabled(true);
        sliderX.setMinorTickSpacing(10);
        sliderX.setPaintTicks(true);
        sliderX.addChangeListener(this);
        sliderX.setVisible(true);

        JLabel labelX1 = new JLabel("out");

        labelX1.setForeground(Color.black);
        labelX1.setFont(MipavUtil.font12);
        labelX1.setEnabled(true);

        JLabel labelXEnd = new JLabel("in");

        labelXEnd.setForeground(Color.black);
        labelXEnd.setFont(MipavUtil.font12);
        labelXEnd.setEnabled(true);

        Hashtable<Integer,JLabel> labelTableX = new Hashtable<Integer,JLabel>();

        labelTableX.put(new Integer(0), labelX1);
        labelTableX.put(new Integer(100), labelXEnd);
        sliderX.setLabelTable(labelTableX);
        sliderX.setPaintLabels(true);
        gbc.gridx = 1;
        gbc.gridy = 0;
        sliderPanel.add(sliderX, gbc);

        // Build x, y, z sliders
        // Labels for the x, y, and z sliders
        JLabel[] sliderLabelsS = new JLabel[3];

        sliderLabelsS[0] = createLabel("-1");
        sliderLabelsS[1] = createLabel("0");
        sliderLabelsS[2] = createLabel("1");

        // Labels for the x, y, and z sliders
        Hashtable<Integer,JLabel> labels = new Hashtable<Integer,JLabel>();

        labels.put(new Integer(-100), sliderLabelsS[0]);
        labels.put(new Integer(0), sliderLabelsS[1]);
        labels.put(new Integer(100), sliderLabelsS[2]);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        JLabel xLabelS = new JLabel("X");

        xLabelS.setFont(MipavUtil.font12);
        xLabelS.setForeground(Color.black);

        xSliderS = new JSlider(-100, 100, 0);
        xSliderS.setFont(MipavUtil.font12);
        xSliderS.setMinorTickSpacing(10);
        xSliderS.setPaintTicks(true);
        xSliderS.addChangeListener(this);
        xSliderS.setLabelTable(labels);
        xSliderS.setPaintLabels(true);
        xSliderS.setAlignmentX(Component.LEFT_ALIGNMENT);
        xLabelS.setAlignmentX(Component.LEFT_ALIGNMENT);

        gbc.gridx = 0;
        gbc.gridy = 2;
        sliderPanel.add(xLabelS, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        sliderPanel.add(xSliderS, gbc);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        JLabel yLabelS = new JLabel("Y");

        yLabelS.setFont(MipavUtil.font12);
        yLabelS.setForeground(Color.black);

        ySliderS = new JSlider(-100, 100, 0);
        ySliderS.setFont(MipavUtil.font12);
        ySliderS.setMinorTickSpacing(10);
        ySliderS.setPaintTicks(true);
        ySliderS.addChangeListener(this);
        ySliderS.setLabelTable(labels);
        ySliderS.setPaintLabels(true);
        ySliderS.setAlignmentX(Component.LEFT_ALIGNMENT);
        yLabelS.setAlignmentX(Component.LEFT_ALIGNMENT);

        gbc.gridx = 0;
        gbc.gridy = 3;
        sliderPanel.add(yLabelS, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        sliderPanel.add(ySliderS, gbc);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        JLabel zLabelS = new JLabel("Z");

        zLabelS.setFont(MipavUtil.font12);
        zLabelS.setForeground(Color.black);

        zSliderS = new JSlider(-100, 100, 0);
        zSliderS.setFont(MipavUtil.font12);
        zSliderS.setMinorTickSpacing(10);
        zSliderS.setPaintTicks(true);
        zSliderS.addChangeListener(this);
        zSliderS.setLabelTable(labels);
        zSliderS.setPaintLabels(true);
        zSliderS.setAlignmentX(Component.LEFT_ALIGNMENT);
        zLabelS.setAlignmentX(Component.LEFT_ALIGNMENT);

        gbc.gridx = 0;
        gbc.gridy = 4;
        sliderPanel.add(zLabelS, gbc);

        gbc.gridx = 1;
        gbc.gridy = 4;
        sliderPanel.add(zSliderS, gbc);

        return sliderPanel;
    }

    /**
     * Build the four tabbed panes, probe control pane, burn control pane, burn parameter control pane, and demo pane.
     */
    private void buildTabbedPanel() {
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addChangeListener(this);

        tabPanel = new JPanel(new BorderLayout());
        tabPanel.add(tabbedPane);

        buildTargetPanel();
        buildProbePlacementPanel();
        buildTreatmentPanel();
        buildProbeModelPanel();
        buildBurnVisPanel();
        buildBurnPackingPanel();
    }

    /**
     * Build the target surface (tumor) control panel. This panel can add or remove surfaces, also changes the surface
     * opacity.
     *
     * @return  the target surface list panel.
     */
    private JPanel buildTargetListPanel() {
        targetList = new JList();

        JScrollPane kScrollPane = new JScrollPane(targetList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel targetListPanel = new JPanel();

        targetListPanel.setLayout(new BorderLayout());

        JPanel buttonPanel = new JPanel(new GridLayout());
        JButton addSurface = new JButton("Add");

        addSurface.addActionListener(this);
        addSurface.setActionCommand("addTarget");
        addSurface.setFont(MipavUtil.font12B);
        addSurface.setMinimumSize(MipavUtil.defaultButtonSize);

        JButton removeSurface = new JButton("Remove");

        removeSurface.addActionListener(this);
        removeSurface.setActionCommand("removeTarget");
        removeSurface.setFont(MipavUtil.font12B);
        removeSurface.setMinimumSize(MipavUtil.defaultButtonSize);

        JButton centerOnSurface = new JButton("Target center");

        centerOnSurface.addActionListener(this);
        centerOnSurface.setActionCommand("centerOnSurface");
        centerOnSurface.setFont(MipavUtil.font12B);
        centerOnSurface.setMinimumSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(addSurface);
        buttonPanel.add(removeSurface);
        buttonPanel.add(centerOnSurface);

        targetListPanel.add(scrollPanel, BorderLayout.CENTER);
        targetListPanel.add(buttonPanel, BorderLayout.SOUTH);
        targetListPanel.setBorder(MipavUtil.buildTitledBorder("Target surface list"));

        return targetListPanel;
    }

    /**
     * Build the tab which allows the user to pick the current tumor to target, to load/remove tumor surfaces and show
     * tumor position and volume info.
     */
    private void buildTargetPanel() {
        JPanel targetOptionPanel = new JPanel(new GridLayout(0, 1));

        targetOptionPanel.setBorder(MipavUtil.buildTitledBorder("Targeting options"));
        snapToTargetCB = new JCheckBox("Snap to center of target surface when selection changes.", snapToTarget);
        snapToTargetCB.addActionListener(this);
        snapToTargetCB.setActionCommand("snapToTarget");
        snapToTargetCB.setFont(MipavUtil.font12B);
        targetOptionPanel.add(snapToTargetCB);

        JPanel infoPanel = new JPanel(new GridLayout(2, 1));

        infoPanel.setBorder(MipavUtil.buildTitledBorder("Target surface information"));

        JPanel centerPanel = new JPanel(new GridLayout(1, 0));

        centerPanel.add(createBoldLabel("Center: "));
        centerPanel.add(createBoldLabel("X: "));
        targetXText = new JTextField(5);
        targetXText.setEditable(false);
        targetXText.setBorder(new EmptyBorder(targetXText.getInsets()));
        targetXText.setAlignmentX(Component.LEFT_ALIGNMENT);
        centerPanel.add(targetXText);
        centerPanel.add(createBoldLabel("Y: "));
        targetYText = new JTextField(5);
        targetYText.setEditable(false);
        targetYText.setBorder(new EmptyBorder(targetYText.getInsets()));
        targetYText.setAlignmentX(Component.LEFT_ALIGNMENT);
        centerPanel.add(targetYText);
        centerPanel.add(createBoldLabel("Z: "));
        targetZText = new JTextField(5);
        targetZText.setEditable(false);
        targetZText.setBorder(new EmptyBorder(targetZText.getInsets()));
        targetZText.setAlignmentX(Component.LEFT_ALIGNMENT);
        centerPanel.add(targetZText);
        infoPanel.add(centerPanel);

        JPanel statsPanel = new JPanel(new GridLayout(1, 0));

        statsPanel.add(createBoldLabel("Volume: "));
        targetVolumeText = new JTextField(5);
        targetVolumeText.setEditable(false);
        targetVolumeText.setBorder(new EmptyBorder(targetVolumeText.getInsets()));
        targetVolumeText.setAlignmentX(Component.LEFT_ALIGNMENT);
        statsPanel.add(targetVolumeText);
        infoPanel.add(statsPanel);

        JPanel targetPanel = new JPanel();

        targetPanel.setLayout(new BoxLayout(targetPanel, BoxLayout.Y_AXIS));
        targetPanel.add(buildTargetListPanel());
        targetPanel.add(infoPanel);
        targetPanel.add(targetOptionPanel);
        tabbedPane.addTab("Targeting", null, targetPanel);
    }

    /**
     * Build the probe toolbar control.
     */
    private void buildToolBar() {

        ViewToolBarBuilder toolbarBuilder = new ViewToolBarBuilder(this);
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 0;
        gbc.gridy = 0;

        Border etchedBorder = BorderFactory.createEtchedBorder();

        toolBar = new JToolBar();
        toolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        toolBar.setBorder(etchedBorder);
        toolBar.setFloatable(false);

        ButtonGroup probeGroup = new ButtonGroup();
        toolBar.add(toolbarBuilder.buildToggleButton("Navigation", "Navigation Mode", "probe", probeGroup));
        toolBar.add(toolbarBuilder.buildButton("Burn", "Burning", "burn"));

        ButtonGroup cursorGroup = new ButtonGroup();
        traverseButton = toolbarBuilder.buildToggleButton("traverse", "traverse image", "translate", cursorGroup);
        toolBar.add(traverseButton);
        probeTargetButton = toolbarBuilder.buildToggleButton("ProbeTargetPoint",
                                                             "Picks a probe target point using the Tri-Planar views",
                                                             "probepoint", cursorGroup);
        toolBar.add(probeTargetButton);
        probeResetButton = toolbarBuilder.buildToggleButton("ProbeReset", "Resets the Probe navigation", "probepoint",
                                                            cursorGroup);
        toolBar.add(probeResetButton);
        toolBar.add(ViewToolBarBuilder.makeSeparator());
        skinPresetButton = toolbarBuilder.buildButton("SkinPreset", "Skin visualization settings.", "histolut");
        toolBar.add(skinPresetButton);
        liverPresetButton = toolbarBuilder.buildButton("LiverPreset", "Liver visualization settings.", "histolut");
        toolBar.add(liverPresetButton);
        bonePresetButton = toolbarBuilder.buildButton("BonePreset", "Bone visualization settings.", "histolut");
        toolBar.add(bonePresetButton);
        loadVisualizationButton = toolbarBuilder.buildButton("LoadPreset", "Load custom visualization settings.",
                                                             "defaultlutopen");
        toolBar.add(loadVisualizationButton);
        saveVisualizationButton = toolbarBuilder.buildButton("SavePreset", "Save custom visualization settings.",
                                                             "defaultlutsave");
        toolBar.add(saveVisualizationButton);
        mainPanel.add(toolBar, gbc);
    }

    /**
     * Build the tab containing ablation treatment information, including a list of burns; burn removal, position and
     * volume; comparison of current tumor vs current set of burns.
     */
    private void buildTreatmentPanel() {
        burnList = new JList();
        burnList.addListSelectionListener(this);

        JScrollPane kScrollPane = new JScrollPane(burnList);
        JPanel scrollPanel = new JPanel();

        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane);
        scrollPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel burnListPanel = new JPanel();

        burnListPanel.setLayout(new BorderLayout());

        JPanel buttonPanel = new JPanel();

        JButton removeSurface = new JButton("remove");

        removeSurface.addActionListener(this);
        removeSurface.setActionCommand("removeBurn");
        removeSurface.setFont(MipavUtil.font12B);
        removeSurface.setMinimumSize(MipavUtil.defaultButtonSize);

        JButton removeAllSurface = new JButton("removeAll");

        removeAllSurface.addActionListener(this);
        removeAllSurface.setActionCommand("removeAllBurn");
        removeAllSurface.setFont(MipavUtil.font12B);
        removeAllSurface.setMinimumSize(MipavUtil.defaultButtonSize);

        buttonPanel.add(removeSurface);
        buttonPanel.add(removeAllSurface);

        burnListPanel.add(scrollPanel, BorderLayout.CENTER);
        burnListPanel.add(buttonPanel, BorderLayout.SOUTH);
        burnListPanel.setBorder(MipavUtil.buildTitledBorder("Burn list"));

        // burn info

        // Center coordinate panel
        centerLabel = new JLabel("Center Coordinate     ");
        centerLabel.setFont(MipavUtil.font12B);
        centerLabel.setForeground(Color.black);

        JPanel xyzPanel = new JPanel();

        xyzPanel.setLayout(new BoxLayout(xyzPanel, BoxLayout.X_AXIS));

        xLabel = new JLabel("X: ");
        xLabel.setFont(MipavUtil.font12B);
        xLabel.setForeground(Color.black);
        xLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        xText = new JTextField(5);
        xText.setEditable(false);
        xText.setBorder(new EmptyBorder(xText.getInsets()));
        xText.setAlignmentX(Component.LEFT_ALIGNMENT);

        yLabel = new JLabel("Y: ");
        yLabel.setFont(MipavUtil.font12B);
        yLabel.setForeground(Color.black);
        yLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        yText = new JTextField(5);
        yText.setEditable(false);
        yText.setBorder(new EmptyBorder(yText.getInsets()));
        yText.setAlignmentX(Component.LEFT_ALIGNMENT);

        zLabel = new JLabel("Z: ");
        zLabel.setFont(MipavUtil.font12B);
        zLabel.setForeground(Color.black);
        zLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        zText = new JTextField(5);
        zText.setEditable(false);
        zText.setBorder(new EmptyBorder(zText.getInsets()));
        zText.setAlignmentX(Component.LEFT_ALIGNMENT);

        xyzPanel.add(xLabel);
        xyzPanel.add(xText);
        xyzPanel.add(yLabel);
        xyzPanel.add(yText);
        xyzPanel.add(zLabel);
        xyzPanel.add(zText);

        JPanel centerPanel = new JPanel();

        centerPanel.setLayout(new BoxLayout(centerPanel, BoxLayout.X_AXIS));
        centerPanel.add(centerLabel);
        centerPanel.add(xyzPanel);

        JPanel paramPanel = new JPanel();

        paramPanel.setLayout(new BoxLayout(paramPanel, BoxLayout.Y_AXIS));

        JPanel radiusPanel = new JPanel();

        radiusPanel.setLayout(new BoxLayout(radiusPanel, BoxLayout.X_AXIS));

        radiusLabel = new JLabel("Burning radius     ");
        radiusLabel.setFont(MipavUtil.font12B);
        radiusLabel.setForeground(Color.black);
        radiusLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiXLabel = new JLabel("semiX: ");
        semiXLabel.setFont(MipavUtil.font12B);
        semiXLabel.setForeground(Color.black);
        semiXLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiXText = new JTextField(5);
        semiXText.setEditable(false);
        semiXText.setBorder(new EmptyBorder(semiXText.getInsets()));
        semiXText.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiYLabel = new JLabel("semiY: ");
        semiYLabel.setFont(MipavUtil.font12B);
        semiYLabel.setForeground(Color.black);
        semiYLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiYText = new JTextField(5);
        semiYText.setEditable(false);
        semiYText.setBorder(new EmptyBorder(semiYText.getInsets()));
        semiYText.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiZLabel = new JLabel("semiZ: ");
        semiZLabel.setFont(MipavUtil.font12B);
        semiZLabel.setForeground(Color.black);
        semiZLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        semiZText = new JTextField(5);
        semiZText.setEditable(false);
        semiZText.setBorder(new EmptyBorder(semiZText.getInsets()));
        semiZText.setAlignmentX(Component.LEFT_ALIGNMENT);

        radiusPanel.add(radiusLabel);
        radiusPanel.add(semiXLabel);
        radiusPanel.add(semiXText);
        radiusPanel.add(semiYLabel);
        radiusPanel.add(semiYText);
        radiusPanel.add(semiZLabel);
        radiusPanel.add(semiZText);

        // entry point panel
        entryPointLabel = new JLabel("Entry Point     ");
        entryPointLabel.setFont(MipavUtil.font12B);
        entryPointLabel.setForeground(Color.black);

        JPanel entryPointPanel = new JPanel();

        entryPointPanel.setLayout(new BoxLayout(entryPointPanel, BoxLayout.X_AXIS));

        xLabelEntry = new JLabel("X: ");
        xLabelEntry.setFont(MipavUtil.font12B);
        xLabelEntry.setForeground(Color.black);
        xLabelEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        xTextEntry = new JTextField(5);
        xTextEntry.setEditable(false);
        xTextEntry.setBorder(new EmptyBorder(xText.getInsets()));
        xTextEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        yLabelEntry = new JLabel("Y: ");
        yLabelEntry.setFont(MipavUtil.font12B);
        yLabelEntry.setForeground(Color.black);
        yLabelEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        yTextEntry = new JTextField(5);
        yTextEntry.setEditable(false);
        yTextEntry.setBorder(new EmptyBorder(yText.getInsets()));
        yTextEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        zLabelEntry = new JLabel("Z: ");
        zLabelEntry.setFont(MipavUtil.font12B);
        zLabelEntry.setForeground(Color.black);
        zLabelEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        zTextEntry = new JTextField(5);
        zTextEntry.setEditable(false);
        zTextEntry.setBorder(new EmptyBorder(zText.getInsets()));
        zTextEntry.setAlignmentX(Component.LEFT_ALIGNMENT);

        entryPointPanel.add(xLabelEntry);
        entryPointPanel.add(xTextEntry);
        entryPointPanel.add(yLabelEntry);
        entryPointPanel.add(yTextEntry);
        entryPointPanel.add(zLabelEntry);
        entryPointPanel.add(zTextEntry);

        JPanel entryPanel = new JPanel();

        entryPanel.setLayout(new BoxLayout(entryPanel, BoxLayout.X_AXIS));
        entryPanel.add(entryPointLabel);
        entryPanel.add(entryPointPanel);

        volumeLabel = new JLabel("Burning Volume");
        volumeLabel.setFont(MipavUtil.font12B);
        volumeLabel.setForeground(Color.black);
        volumeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        volumeText = new JTextField(10);
        volumeText.setEditable(false);
        volumeText.setBorder(new EmptyBorder(volumeText.getInsets()));
        volumeText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel volumePanel = new JPanel();

        volumePanel.setLayout(new BoxLayout(volumePanel, BoxLayout.X_AXIS));
        volumePanel.add(volumeLabel);
        volumePanel.add(volumeText);

        totalVolumeLabel = new JLabel("Total Burn Volume");
        totalVolumeLabel.setFont(MipavUtil.font12B);
        totalVolumeLabel.setForeground(Color.black);
        totalVolumeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        totalVolumeText = new JTextField(10);
        totalVolumeText.setEditable(false);
        totalVolumeText.setBorder(new EmptyBorder(totalVolumeText.getInsets()));
        totalVolumeText.setAlignmentX(Component.LEFT_ALIGNMENT);

        calcButton = new JButton(MipavUtil.getIcon("calculator.gif"));
        calcButton.addActionListener(this);
        calcButton.setToolTipText("Calculate Burning total volume");
        calcButton.setActionCommand("calcVolume");
        calcButton.setBorderPainted(false);
        calcButton.setRolloverEnabled(true);
        calcButton.setRolloverIcon(MipavUtil.getIcon("calculatorroll.gif"));
        calcButton.setFocusPainted(false);
        calcButton.setEnabled(true);

        JPanel totalVolumePanel = new JPanel();

        totalVolumePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        totalVolumePanel.add(totalVolumeLabel);
        totalVolumePanel.add(calcButton);
        totalVolumePanel.add(totalVolumeText);
        totalVolumePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        diffVolumeLabel = new JLabel("Volume difference");
        diffVolumeLabel.setFont(MipavUtil.font12B);
        diffVolumeLabel.setForeground(Color.black);
        diffVolumeLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        diffVolumeText = new JTextField(10);
        diffVolumeText.setEditable(false);
        diffVolumeText.setBorder(new EmptyBorder(totalVolumeText.getInsets()));
        diffVolumeText.setAlignmentX(Component.LEFT_ALIGNMENT);

        JPanel diffVolumePanel = new JPanel();

        diffVolumePanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        diffVolumePanel.add(diffVolumeLabel);
        diffVolumePanel.add(diffVolumeText);
        diffVolumePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        paramPanel.add(radiusPanel);
        paramPanel.add(volumePanel);
        paramPanel.add(entryPanel);

        JPanel burnParamPanel = new JPanel(new GridBagLayout());

        burnParamPanel.setBorder(MipavUtil.buildTitledBorder("Burn information"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        burnParamPanel.add(centerPanel, gbc);
        gbc.gridy++;
        burnParamPanel.add(paramPanel, gbc);

        // tumor targeting info
        JPanel treatmentInfoPanel = new JPanel(new GridLayout(0, 1));

        treatmentInfoPanel.setBorder(MipavUtil.buildTitledBorder("Treatment information"));
        treatmentInfoPanel.add(totalVolumePanel);
        treatmentInfoPanel.add(diffVolumePanel);

        JPanel treatmentPanel = new JPanel();

        treatmentPanel.setLayout(new BoxLayout(treatmentPanel, BoxLayout.Y_AXIS));
        treatmentPanel.add(burnListPanel);
        treatmentPanel.add(burnParamPanel);
        treatmentPanel.add(treatmentInfoPanel);
        tabbedPane.addTab("Treatment", null, treatmentPanel);
    }

    /**
     * Enable or disable the components on the target treatment panel.
     *
     * @param  flag  whether to enable the components
     */
    private void enableBurnTreatmentComponents(boolean flag) {

        // burn info components
        centerLabel.setEnabled(flag);
        xLabel.setEnabled(flag);
        xText.setEnabled(flag);
        yLabel.setEnabled(flag);
        yText.setEnabled(flag);
        zLabel.setEnabled(flag);
        zText.setEnabled(flag);
        entryPointLabel.setEnabled(flag);
        xLabelEntry.setEnabled(flag);
        xTextEntry.setEnabled(flag);
        yLabelEntry.setEnabled(flag);
        yTextEntry.setEnabled(flag);
        zLabelEntry.setEnabled(flag);
        zTextEntry.setEnabled(flag);
        radiusLabel.setEnabled(flag);
        semiXLabel.setEnabled(flag);
        semiYLabel.setEnabled(flag);
        semiZLabel.setEnabled(flag);
        semiXText.setEnabled(flag);
        semiYText.setEnabled(flag);
        semiZText.setEnabled(flag);
        volumeLabel.setEnabled(flag);
        volumeText.setEnabled(flag);
        totalVolumeText.setEnabled(flag);
        totalVolumeLabel.setEnabled(flag);
        diffVolumeText.setEnabled(flag);
        diffVolumeLabel.setEnabled(flag);
        calcButton.setEnabled(flag);
    }

    /**
     * Enable or disable the components on the burn visualization panel.
     *
     * @param  flag  whether to enable the components
     */
    private void enableBurnVisComponents(boolean flag) {
        colorLabel.setEnabled(flag);
        colorButton.setEnabled(flag);
        opacityLabel.setEnabled(flag);
        opacitySliderLabels[0].setEnabled(flag);
        opacitySliderLabels[1].setEnabled(flag);
        opacitySliderLabels[2].setEnabled(flag);
        opacitySlider.setEnabled(flag);
        burnClipCB.setEnabled(flag);
        burnBackFaceCB.setEnabled(flag);
        showBurnLabelsCB.setEnabled(flag);
    }

    /**
     * This is called when the user chooses a new color for the surface. It changes the color of the surface.
     *
     * @param  color  Color to change surface to.
     */
    private void getColorChange(Color color) {

        // change the material colors for the selected items
        int[] aiSelected = burnList.getSelectedIndices();

        for (int i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];

            Color4f newColor = new Color4f(color);

            // Color3f changeColor = new Color3f(color);

            getCurTreatment().getBurn(iIndex).color = newColor;

            // change the material color
            BranchGroup root = getCurTreatment().getBurn(iIndex).burnBG;

            Shape3D shape = (Shape3D) (((BranchGroup) (root.getChild(0))).getChild(0));

            Appearance appearance = shape.getAppearance();
            Material mat = new Material(new Color3f(color), new Color3f(color), new Color3f(color), new Color3f(color),
                                        80f);

            mat.setCapability(Material.ALLOW_COMPONENT_WRITE);
            appearance.setMaterial(mat);
        }
    }

    /**
     * Get information about a treatment.
     *
     * @param   index  the index of the treatment to retreive
     *
     * @return  info about a treatment
     */
    private TreatmentInformation getTreatmentInfo(int index) {
        return (TreatmentInformation) treatmentVector.get(index);
    }

    /**
     * Initialize the whole probe panel control layout.
     */
    private void init() {
        mainPanel = new JPanel(new GridBagLayout());

        drawingPane = new DrawingPane();
        drawingPane.setBackground(Color.white);
        drawingPane.setLayout(new GridBagLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(drawingPane, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;

        buildToolBar();

        buildTabbedPanel();
        gbc.gridx = 0;
        gbc.gridy = 1;
        drawingPane.add(tabPanel, gbc);

        enableBurnVisComponents(false);
        enableBurnTreatmentComponents(false);

        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(scroller, gbc);
    }

    /**
     * Ask the user for an image to load and return it.
     *
     * @return  the image the user wants to load
     */
    private ModelImage loadImage() {
        ViewOpenFileUI openFile = new ViewOpenFileUI(true);

        openFile.setPutInFrame(false);

        // set the filter type to the preferences saved filter
        int filter = ViewImageFileFilter.TECH;

        try {
            filter = Integer.parseInt(Preferences.getProperty(Preferences.PREF_FILENAME_FILTER));
        } catch (NumberFormatException nfe) {

            // an invalid value was set in preferences -- so fix it!
            filter = ViewImageFileFilter.TECH;
            Preferences.setProperty(Preferences.PREF_FILENAME_FILTER, Integer.toString(filter));
        }

        openFile.setFilterType(filter);


        // This arrayLIst allows multiple selections but i am only accessing the zeroth element
        // to load the region map
        ArrayList<Vector<String>> openImagesArrayList = openFile.open(false);

        // if open failed, then imageNames will be null
        Vector<String> imageNames = (Vector<String>) openImagesArrayList.get(0);

        if (imageNames == null) {
            return null;
        }

        // get the first image
        String name = (String) imageNames.get(0);
        ModelImage img = parentFrame.getUserInterface().getRegisteredImageByName(name);

        return img;
    }

    /**
     * Loads visualization settings for this image.
     *
     * @param  lut   the lut transfer function
     * @param  opac  the opacity transfer function
     */
    private void loadVisualizationSettings(TransferFunction lut, TransferFunction opac) {
        boolean imageBLoaded = parentFrame.getImageB() != null;

        parentFrame.getHistogramDialog().setTransferFunctionA(lut);
        //((ViewJComponentHistoLUT) parentFrame.getLUTDialog().getHistoLUTComponentA()).getLUT().setTransferFunction(lut);

        if (imageBLoaded) {
            parentFrame.getHistogramDialog().setTransferFunctionB(lut);
            //((ViewJComponentHistoLUT) parentFrame.getLUTDialog().getHistoLUTComponentB()).getLUT().setTransferFunction(lut);
        }

        if (((SurfaceRender) renderBase).getDisplayMode3D()) {
            ((SurfaceRender) renderBase).getVolOpacityPanel().getSelectedComponent(JPanelVolOpacity.COMP_A).updateTransFunc(opac);

            if (imageBLoaded) {
                ((SurfaceRender) renderBase).getVolOpacityPanel().getSelectedComponent(JPanelVolOpacity.COMP_B).updateTransFunc(opac);
            }
        }

        parentFrame.volumeRepaint();
    }

    /**
     * Remove all the burning point geometry shapes in the volume.
     */
    private void removeAllBurn() {
        burnBase.getBurnRootParentBG().removeAllChildren();
        getCurTreatment().removeAllBurns();
        burnBase.removeAllBurnCenters();

        Vector<String> burnNames = new Vector<String>();

        burnList.setListData(burnNames);
        enableBurnVisComponents(false);
        enableBurnTreatmentComponents(false);
        colorButton.setBackground(getBackground());
        xText.setText("");
        yText.setText("");
        zText.setText("");
        xTextEntry.setText("");
        yTextEntry.setText("");
        zTextEntry.setText("");
        volumeText.setText("");
        totalVolumeText.setText("");
        diffVolumeText.setText("");
        semiXText.setText("");
        semiYText.setText("");
        semiZText.setText("");
    }

    /**
     * The action taken when the Remove button is clicked in the list box. All selected surfaces in the list box are
     * removed from the scene graph.
     */
    private void removeBurn() {

        // construct the lists of items to be removed
        int[] aiSelected = burnList.getSelectedIndices();

        Vector<BurnAttributes> removeBurns = new Vector<BurnAttributes>();
        int i;

        for (i = 0; i < aiSelected.length; i++) {
            int iIndex = aiSelected[i];

            removeBurns.add(getCurTreatment().getBurn(iIndex));
        }

        // remove the items
        for (i = 0; i < aiSelected.length; i++) {
            BranchGroup root = ((BurnAttributes) removeBurns.get(i)).burnBG;

            for (int j = 0; j < burnBase.getBurnRootParentBG().numChildren(); j++) {

                if (burnBase.getBurnRootParentBG().getChild(j) == root) {
                    burnBase.getBurnRootParentBG().removeChild(j);
                    burnBase.removeBurnCenter(j);
                }
            }

            getCurTreatment().removeBurn(removeBurns.get(i));
        }

        Vector<String> burnNames = new Vector<String>();

        for (Enumeration<BurnAttributes> en = getCurTreatment().getBurnEnum(); en.hasMoreElements();) {
            burnNames.addElement(((BurnAttributes) en.nextElement()).name);
        }

        burnList.setListData(burnNames);

        // determine the new selected list item
        if (burnNames.size() > 0) {

            // highlight another list item when current one is removed
            burnList.setSelectedIndex(burnNames.size() - 1);

            // update the burn info components
            updateTreatmentPanel(false);
        } else {

            // clear out the color values if the list is empty
            enableBurnVisComponents(false);
            enableBurnTreatmentComponents(false);

            colorButton.setBackground(getBackground());
            xText.setText("");
            yText.setText("");
            zText.setText("");
            xTextEntry.setText("");
            yTextEntry.setText("");
            zTextEntry.setText("");
            volumeText.setText("");
            totalVolumeText.setText("");
            diffVolumeText.setText("");
            semiXText.setText("");
            semiYText.setText("");
            semiZText.setText("");
        }
    }

    /**
     * Ask the user for a name to save an image as and then write it out to disk.
     *
     * @param  img  the image to save
     */
    private void saveImage(ModelImage img) {

        // set up save options
        FileWriteOptions options = new FileWriteOptions("", "", true);

        options.setFileType(FileUtility.XML);

        if (img.getNDims() == 3) {
            options.setBeginSlice(0);
            options.setEndSlice(img.getExtents()[2] - 1);
        }

        options.setSaveAs(true);

        // save the image
        String fileName = null;
        String extension = null;
        String directory = null;
        String suffix = null;
        int fileType = FileUtility.UNDEFINED;
        ViewImageFileFilter vFilter = null;
        int i;
        int filterType = -1;
        boolean operateQuiet = false;

        // System.err.println( "Save image (base) saveAs: " + options.isSaveAs() + " is set: " + options.isSet());

        if (options.isSaveAs()) {

            // save into its own subdirectory when on SaveAs.
            // (preferrably used in multi-file formats., ie DICOM)
            options.setSaveInSubdirectory(true);

            if (options.isSet()) {
                fileName = options.getFileName();
                directory = options.getFileDirectory();
            } else {

                try {
                    JFileChooser chooser = new JFileChooser();

                    if (parentFrame.getUserInterface().getDefaultDirectory() != null) {
                        chooser.setCurrentDirectory(new File(parentFrame.getUserInterface().getDefaultDirectory()));
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

                        if (options.isAVI()) {

                            // force the name to be .avi
                            if (!fileName.endsWith("avi") && !fileName.endsWith("AVI")) {
                                fileName += ".avi";
                            }
                        }

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
                        parentFrame.getUserInterface().setDefaultDirectory(directory);
                    } else {
                        return;
                    }
                } catch (OutOfMemoryError error) {

                    if (!operateQuiet) {
                        MipavUtil.displayError("Out of memory: ViewJFrameBase.save");
                    }

                    Preferences.debug("Out of memory: ViewJFrameBase.save\n", Preferences.DEBUG_COMMS);

                    return;
                }
            }
        } else {
            fileName = img.getFileInfo(0).getFileName();
            directory = img.getFileInfo(0).getFileDirectory();
        }

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

            fileIO.setQuiet(operateQuiet);

            fileIO.writeImage(img, options);
        }

        // set the new fileName and directory in the fileInfo for the img -- so that it's
        // updated correctly in memory as well -- don't move this before the saveAllOnSave loop --
        // that needs to look at the former settings!
        FileInfoBase[] fileInfo = img.getFileInfo();

        if (suffix == null) {
            suffix = FileUtility.getExtension(fileName);
            fileType = FileUtility.getFileType(fileName, directory, operateQuiet);
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
     * Start to try to choose an optimal ablation plan for the current target surface.
     */
    private void startBurnPacking() {
        System.out.println("StartBurnPacking()  BEGIN");

        // BitSet surfaceMask = ( (SurfaceRender) renderBase ).getSurfaceDialog().getSurfaceMask();

        System.out.println("StartBurnPacking()  DONE");
    }

    /**
     * Update the labels which show info about the current target surface.
     */
    private void updateTargetLabels() {
        Vector<SurfaceAttributes> surfaceVector = (Vector<SurfaceAttributes>) ((SurfaceRender) renderBase).getSurfaceDialog().getSurfaceVector().clone();

        for (int i = 0; i < surfaceVector.size(); i++) {
            String name = ((SurfaceAttributes) surfaceVector.get(i)).getName();

            if (name.endsWith("_liver.sur") || name.endsWith("_vasculature.sur")) {
                surfaceVector.remove(i);
                i--;
            }
        }

        SurfaceAttributes surface = null;
        String selection = (String) targetList.getSelectedValue();

        for (Enumeration<SurfaceAttributes> en = surfaceVector.elements(); en.hasMoreElements();) {
            SurfaceAttributes sur = (SurfaceAttributes) en.nextElement();
            String name = sur.getName();

            if (name.equals(selection)) {
                surface = sur;
            }
        }

        if (surface != null) {

            // / should these be in voxel space instead of real (eg. mm) space?
            Point3f volPt = transformPtVolumeToImage(surface.getCenter());

            targetXText.setText("" + MipavUtil.makeFloatString(volPt.x * xRes, 3));
            targetYText.setText("" + MipavUtil.makeFloatString(volPt.y * yRes, 3));
            targetZText.setText("" + MipavUtil.makeFloatString(volPt.z * zRes, 3));

            // / assumes that the units are the same in all dims
            float mmVolume = surface.getVolume();
            String unitStr = Unit.getUnitFromLegacyNum(parentFrame.getImageOriginal().getFileInfo(0).getUnitsOfMeasure(0)).getAbbrev();

            targetVolumeText.setText("" + mmVolume + " " + unitStr + "^3");
        } else {
            targetXText.setText("");
            targetYText.setText("");
            targetZText.setText("");
            targetVolumeText.setText("");
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Helper class for resizing.
     */
    public class DrawingPane extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = 199157917084746068L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
        }
    }


    /**
     * Cancel the color dialog, change nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Unchanged.
         *
         * @param  e  action event
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Pick up the selected color and call method to change the surface color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Sets the button color to the chosen color and changes the color of the surface.
         *
         * @param  e  Event that triggered this method.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            colorButton.setBackground(color);
            getColorChange(color);
        }
    }
}
