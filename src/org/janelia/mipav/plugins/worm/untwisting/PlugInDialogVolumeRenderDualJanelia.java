package org.janelia.mipav.plugins.worm.untwisting;


//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.OpenCLInfo;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransferFunction;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.util.MipavInitGPU;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.JPanelVolumeOpacity;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.RendererListener;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRenderBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelClip_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelLights_WM;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.JPanelAnnotations;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.JPanelCurves;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.JPanelLattice;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.VOIWormAnnotation;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import static org.jocl.CL.CL_DEVICE_TYPE_GPU;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Vector;
import java.util.stream.Collectors;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import org.jocl.Sizeof;

import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.Rendering.GraphicsImage;
import WildMagic.LibGraphics.Rendering.Texture;
import WildMagic.LibGraphics.Rendering.WireframeState;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

/**
* Implements the user-interface for the semi-automatic straightening of the
* worm. Provides batch-process algorithms for segmenting the seam cells and
* building lattices. Provides the framework for enabling the user to step
* through the selected image volumes and view/edit results from the automatic
* processes. Provides framework for animating the annotations after untwisting.
*/
public class PlugInDialogVolumeRenderDualJanelia extends JFrame implements ActionListener, RendererListener,
		PropertyChangeListener, ViewImageUpdateInterface, WindowListener, ChangeListener {

	private static final long serialVersionUID = -9056581285643263551L;

	public static final int EditNONE = 0;
	public static final int EditSeamCells = 1;
	public static final int EditLattice = 2;
	public static final int CheckSeam = 3;
	public static final int IntegratedEditing = 6;
	public static final int ReviewResults = 7;

	private JPanel algorithmsPanel;
	private JButton backButton;
	private String[] baseFileDir;
	private String latticeFileDir;
	private JTextField baseFileLocText;
	private String baseFileName;
	private JTextField baseFileNameText;
	private JProgressBar batchProgress;
	private JRadioButton batchFlipLattice;
	private JPanel backNextPanel;
	private JRadioButton calcMaxProjection;
	private JRadioButton resliceRotate;
	private JRadioButton generateModelMesh;
	private JPanel choicePanel;
	private JButton closeButton;
	private JRadioButton createAnimation;
	private GuiBuilder gui;
	private JButton doneButton;
	private JRadioButton editLattice;
	private JRadioButton checkSeamCells;
	private int editMode = EditNONE;
	private JPanel editPanel;
	private JRadioButton editSeamCells;
	private JPanel gpuPanel;
	private int imageIndex = 0;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;
	private JPanel optionsPanel;
	private JPanel displayControls;
	private JPanel imageChannels;

	private JPanel latticeSelectionPanel;
	private JRadioButton latticeStraighten;

	private JTextField seamCellMinRadiusText;
	private JTextField seamCellMaxRadiusText;
	private JTextField segmentationPaddingText;
	private JCheckBox thresholdImageCheck;
	private JTextField thresholdValue;
	private JCheckBox modelStraightenCheck;

	private JCheckBox resliceImageCheck;
	private JTextField resliceX, resliceY, resliceZ;
	private int resliceXValue = 250, resliceYValue = 250, resliceZValue = 1500;

	private int paddingFactor;
	private int minRadius;
	private int maxRadius;
	private int threshold = -1;

	private JTabbedPane lutTab;
	private JPanel lutPanel;

	private JButton newLatticeButton;
	private JButton flipLatticeButton;
	private JButton predict;
	private JButton previewUntwisting;
	private JCheckBox displayModel;
	private JCheckBox editCrossSections;
	private JCheckBox displaySurface;

	private int previewCount = 0;

	private JButton nextButton;
	private int nextDirection = 1;

	private JPanel opacityPanel;
	private JTabbedPane opacityTab;
	private JPanel clipPanel;

	private PlugInDialogVolumeRenderDualJanelia parent;
	private JTextField rangeFusionText;
	private JCheckBox reverseSequence;
	private JCheckBox loadLegacyLatticeCheck;
	private JCheckBox loadLegacyAnnotationsCheck;

	private JRadioButton integratedEdit;
	private JRadioButton reviewResults;

	private JRadioButton segmentSeamCells;

	private JButton startButton;

	private JTabbedPane tabbedPane;
	private int selectedTab = -1;

	private VolumeTriPlanarInterface triVolume;
	private JSplitPane annotationPanels;
	private JPanel annotationPanelSingle;
	private JSplitPane curvePanel;
	private JPanel curvePanelSingle;

	private Container volumePanel;
	private JSplitPane integratedPanel;
	private VolumeTriPlanarRender leftRenderer;
	private VolumeTriPlanarRender rightRenderer;
	private VolumeTriPlanarRender activeRenderer;
	private JSplitPane dualGPU;
	private JPanel leftDisplayPanel;
	private JPanel rightDisplayPanel;

	private VOIVector annotationList;
	private Vector<String> annotationNames;
	private JSplitPane latticePanel = null;
	private JPanel latticePanelSingle = null;

	private class IntegratedWormData {
		private VOIVector annotations;
		private ModelImage wormImage;
		private ModelImage previewImage;
		private ModelImage contourImage;
		private VolumeImage volumeImage;
		private VolumeImage[] previewHS;
		private VolumeImage[] hyperstack;
		private Texture colormap;
		private boolean colorMapInit = false;
		private VOILatticeManagerInterface voiManager;
		private JPanelVolumeOpacity[] volOpacityPanel;
		private JFrameHistogram[] lutHistogramPanel;
		private JPanelAnnotations annotationPanelUI;
		private JPanelClip_WM clipGUI;

		private VOI annotationsTwisted = null;
		private VOIVector latticeTwisted = null;
		private boolean annotationOpen = true;

		private JPanelCurves curvesPanelUI;

		private JPanelLattice latticeTable = null;

		private Matrix3f volumeMatrix = new Matrix3f();
		private Matrix3f clipArb = null;
		private boolean clipArbOn = false;

		private int currentTab = -1;

		public IntegratedWormData() {
		}
	};

	// hyperstack arrays:
	private IntegratedWormData[] imageStack = null;
	private IntegratedWormData leftImage = null;
	private IntegratedWormData rightImage = null;
	private IntegratedWormData activeImage = null;

	private Dimension currentSize = null;

	public PlugInDialogVolumeRenderDualJanelia() {
		this.editMode = EditNONE;
		init();
		setVisible(true);
		addWindowListener(this);
	}

	private boolean nextBackFlag = false;
	private int nextBackCount = 0;

	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		Object source = event.getSource();
		
		if ( command.equals("BrowseConclude") ) {
			UntwistDialog inputs = new UntwistDialog(baseFileLocText.getText());
			if ( inputs.latticeOutputDir != null ) {
				latticeFileDir = new String(inputs.latticeOutputDir.getText());
				setDefaultInputList(latticeFileDir);
			}
		}
		
		if (currentSize == null) {
			currentSize = new Dimension(getSize());
			System.err.println(currentSize);
		}

		if (editMode == EditNONE) {
			setVariables();
			if (command.equals("start")) {
				if (includeRange == null) {
					if (createAnimation.isSelected()) {
						// Launch the animation tool
						this.setVisible(false);
						annotationAnimationFromSpreadSheet();
						return;
					} 
					else {					
//						this.setVisible(false);
//						testForceGraph();
						MipavUtil.displayError("Please specify a range of images.");
						return;
					}
				}
				startButton.setEnabled(false);
				if (batchFlipLattice.isSelected()) {
					try {
						// Batch Automatic Lattice-Building
						PlugInAlgorithmWormUntwistingJanelia.batchFlipLattices(batchProgress, includeRange, latticeFileDir,
								baseFileNameText.getText());
					} catch (java.lang.OutOfMemoryError e) {
						MipavUtil
								.displayError("Error: Not enough memory. Unable to finish automatic lattice-building.");
						return;
					}
					startButton.setEnabled(true);
				} else if (latticeStraighten.isSelected()) {
					try {
						// Batch Untwisting:
						PlugInAlgorithmWormUntwistingJanelia.latticeStraighten(batchProgress, includeRange, baseFileDir,
								baseFileNameText.getText(), latticeFileDir, paddingFactor,
								modelStraightenCheck.isSelected());
					} catch (java.lang.OutOfMemoryError e) {
						MipavUtil.displayError("Error: Not enough memory. Unable to finish straightening.");
						e.printStackTrace();
						return;
					}
					reviewResults.setSelected(true);
					startButton.setEnabled(true);
				} else if (calcMaxProjection.isSelected()) {
					try {
						// Batch Registration/MP calculation:
						PlugInAlgorithmWormUntwistingJanelia.createMaximumProjectionAVI(batchProgress, includeRange,
								baseFileDir, baseFileNameText.getText());
					} catch (java.lang.OutOfMemoryError e) {
						MipavUtil.displayError(
								"Error: Not enough memory. Unable to finish maximum-projection calculation.");
						return;
					}
					startButton.setEnabled(true);
				} else if (resliceRotate.isSelected()) {
					try {
						if (resliceImageCheck.isSelected()) {
							PlugInAlgorithmWormUntwistingJanelia.reslice(batchProgress, includeRange, baseFileDir,
									baseFileNameText.getText(), resliceXValue, resliceYValue,
									resliceZValue);
						}
					} catch (java.lang.OutOfMemoryError e) {
						MipavUtil.displayError("Error: Not enough memory. Unable to finish reslice calculation.");
						return;
					}
					startButton.setEnabled(true);
				} else if (generateModelMesh.isSelected()) {
					try {
						PlugInAlgorithmWormUntwistingJanelia.generateModelMesh(batchProgress, includeRange, baseFileDir,
								baseFileNameText.getText(), paddingFactor);
					} catch (java.lang.OutOfMemoryError e) {
						MipavUtil.displayError("Error: Not enough memory. Unable to finish reslice calculation.");
						return;
					}
					startButton.setEnabled(true);
				} else if (editLattice.isSelected()) {
					// start lattice editing:
					editMode = EditLattice;
					if (openHyperStack()) {
						displayModel.setVisible(true);
						displaySurface.setVisible(true);
						editCrossSections.setVisible(true);
						previewUntwisting.setVisible(true);
						displayControls.setVisible(false);
						imageChannels.setVisible(true);

						validate();
						leftRenderer.setVisible(true);
						if ( rightRenderer != null ) rightRenderer.setVisible(true);
					} else {
						editMode = EditNONE;
						startButton.setEnabled(true);
					}
				} else if (integratedEdit.isSelected()) {
					// start lattice editing:
					editMode = IntegratedEditing;
					if (openHyperStack()) {
						displayModel.setVisible(true);
						displaySurface.setVisible(true);
						editCrossSections.setVisible(true);
						predict.setVisible(true);
						previewUntwisting.setVisible(true);
						displayControls.setVisible(false);		
						imageChannels.setVisible(true);

						validate();
						leftRenderer.setVisible(true);
						if ( rightRenderer != null ) rightRenderer.setVisible(true);
					} else {
						editMode = EditNONE;
						startButton.setEnabled(true);
					}
				} else if (reviewResults.isSelected()) {
					// start viewing untwisted results:
					editMode = ReviewResults;
					if (openHyperStack()) {
						displayModel.setVisible(true);
						displaySurface.setVisible(true);
						editCrossSections.setVisible(true);
						previewUntwisting.setVisible(true);
						displayControls.setVisible(false);
						imageChannels.setVisible(true);

						validate();
						leftRenderer.setVisible(true);
						if ( rightRenderer != null ) rightRenderer.setVisible(true);
					} else {
						editMode = EditNONE;
						startButton.setEnabled(true);
					}
				} else {
					MipavUtil.displayInfo("Please select an action");
					startButton.setEnabled(true);
				}
			}

			baseFileNameText.setEnabled(!createAnimation.isSelected());
			rangeFusionText.setEnabled(!createAnimation.isSelected());
		} else {
			// Edit mode, next and back open the next or previous image in the
			// sequence to edit and opens the associated VOIs.
			if (command.equals("next")) {
				activeImage.clipArb = activeRenderer.getArbitratyClip();
				activeImage.clipArbOn = activeRenderer.getArbitratyClipOn();
				activeImage.currentTab = tabbedPane.getSelectedIndex();

				nextButton.setEnabled(false);
				backButton.setEnabled(false);
				nextBackFlag = true;
				IntegratedWormData activeTemp = activeImage;
				saveAll();
				activeImage = activeTemp;
				nextDirection = 1;
				imageIndex++;
				if (dualGPU != null) {
					nextBackCount = 2;

					System.err.println(imageIndex);
					rightImage = imageStack[imageIndex + 1];

					rightRenderer.displayVOIs(false);
//					rightRenderer.setImages(rightImage.volumeImage);	
					rightRenderer.setHyperStack(rightImage.hyperstack, rightImage.colormap);
					if (rightImage.voiManager == null) {
						rightImage.voiManager = new VOILatticeManagerInterface(null, rightImage.wormImage, null, 0,
								true, null);
						rightImage.voiManager.setImage(rightImage.wormImage, null);
					}
					rightRenderer.setVOILatticeManager(rightImage.voiManager);

					leftImage = imageStack[imageIndex];
					leftRenderer.displayVOIs(false);
//					leftRenderer.setImages(leftImage.volumeImage);
					leftRenderer.setHyperStack(leftImage.hyperstack, leftImage.colormap);
					leftRenderer.resetAxisY();
					leftRenderer.setVOILatticeManager(leftImage.voiManager);
					if (editMode == EditLattice) {
						leftImage.voiManager.editLattice();
						rightImage.voiManager.editLattice();
					}
				}
			} else if (command.equals("back")) {
				activeImage.clipArb = activeRenderer.getArbitratyClip();
				activeImage.clipArbOn = activeRenderer.getArbitratyClipOn();
				activeImage.currentTab = tabbedPane.getSelectedIndex();

				nextButton.setEnabled(false);
				backButton.setEnabled(false);
				nextBackFlag = true;
				IntegratedWormData activeTemp = activeImage;
				saveAll();
				activeImage = activeTemp;
				nextDirection = -1;
				imageIndex--;
				if (dualGPU != null) {
					nextBackCount = 2;

					System.err.println(imageIndex);
					rightImage = imageStack[imageIndex + 1];

					rightRenderer.displayVOIs(false);
//					rightRenderer.setImages(rightImage.volumeImage);
					rightRenderer.setHyperStack(rightImage.hyperstack, rightImage.colormap);

					if (rightImage.voiManager == null) {
						rightImage.voiManager = new VOILatticeManagerInterface(null, rightImage.wormImage, null, 0,
								true, null);
						rightImage.voiManager.setImage(rightImage.wormImage, null);
					}
					rightRenderer.setVOILatticeManager(rightImage.voiManager);

					leftImage = imageStack[imageIndex];

					leftRenderer.displayVOIs(false);
//					leftRenderer.setImages(leftImage.volumeImage);		
					leftRenderer.setHyperStack(leftImage.hyperstack, leftImage.colormap);

					leftRenderer.resetAxisY();
					leftRenderer.setVOILatticeManager(leftImage.voiManager);
					if (editMode == EditLattice) {
						leftImage.voiManager.editLattice();
						rightImage.voiManager.editLattice();
					}
				}
			}
			// Closes the editing:
			else if (command.equals("done")) {
				if (!displayControls.isVisible()) {
					displayControls.setVisible(true);				
					imageChannels.setVisible(false);

					validate();
				}
				saveAll();
				if (parent != null) {
					parent.enableNext(editMode);
					setVisible(false);
					dispose();
				} else {
					enableNext(editMode);
				}
			}
			else if (command.equals("demo")) {
				updateSurfacePanels();
				activeRenderer.addAnimationLattice( activeImage.voiManager.getLattice() );
			}
			// Enables user to generate a new lattice (when none of the automatic ones match
			// well)
			else if (command.equals("newLattice")) {
				if (activeRenderer != null) {
					activeRenderer.removeSurface("worm");
					activeRenderer.displaySurface(false);
				}
				if (activeImage.voiManager != null) {
					activeImage.voiManager.clear3DSelection();
					activeImage.voiManager.setLattice(new VOIVector());
					activeImage.voiManager.editLattice();
				}
				if (editMode != IntegratedEditing) {
					latticeSelectionPanel.removeAll();
					latticeSelectionPanel.add(newLatticeButton);
					latticeSelectionPanel.add(flipLatticeButton);
					latticeSelectionPanel.add(displayModel);
					displayModel.setSelected(false);
					latticeSelectionPanel.add(displaySurface);
					displaySurface.setSelected(false);
					latticeSelectionPanel.add(editCrossSections);
					editCrossSections.setSelected(false);
					latticeSelectionPanel.add(previewUntwisting);
					this.validate();
				}
			} else if (command.equals("flipLattice")) {
				if (activeImage.voiManager != null) {
					activeImage.voiManager.flipLattice();
				}
			} else if (command.equals("displayModel")) {
				if (activeImage.voiManager != null) {
					activeImage.voiManager.showModel(displayModel.isSelected());
					activeRenderer.updateVOIs();
				}
			} else if (command.equals("displaySurface")) {
				if (activeImage.voiManager != null) {
					if (displaySurface.isSelected()) {
						TriMesh mesh = activeImage.voiManager.generateTriMesh(0);
						SurfaceState surface = new SurfaceState(mesh, "worm");
						// surface.Fill = WireframeState.FillMode.FM_LINE;
						activeRenderer.addSurface(surface, false, true);
						activeRenderer.displaySurface(true);
						updateSurfacePanels();
					} else {
						activeRenderer.removeAll("worm");
						activeRenderer.displaySurface(false);
					}
				}
			} else if (command.equals("editCrossSections")) {
				if (activeImage.voiManager != null) {
					activeImage.voiManager.editCrossSections(editCrossSections.isSelected());
					displayModel.setSelected(editCrossSections.isSelected());
					displayModel.setEnabled(!editCrossSections.isSelected());
					activeRenderer.updateVOIs();
				}
			} else if (command.equals("predict")) {
				runPython();
			} else if (command.equals("preview")) {
				// disable controls:
				doneButton.setEnabled(false);
				nextButton.setEnabled(false);
				backButton.setEnabled(false);
				selectedTab = tabbedPane.getSelectedIndex();
//				activeImage.lut = new TransferFunction[activeImage.hyperstack.length];
//				activeImage.opacity = new TransferFunction[activeImage.hyperstack.length];
//				for ( int i = 0; i < activeImage.hyperstack.length; i++ ) {
//					activeImage.lut[i] = ((ModelLUT) activeImage.volumeImage.getLUT()).getTransferFunction();
////					((ModelLUT) activeImage.volumeImage.getLUT()).setTransferFunction(fn);
//
//					activeImage.opacity[i] = activeImage.volOpacityPanel[i].getCompA().getOpacityTransferFunction();
////					activeImage.hyperstack[i].UpdateImages(activeImage.opacity[i], 0, null);
//				}
				
				if (previewUntwisting.getText().equals("preview")) {
					previewUntwisting.setText("return");
					
					previewCount++;
					// save image orientation:
					activeImage.volumeMatrix = new Matrix3f(activeRenderer.GetSceneRotation());

					// save twisted annotations and lattice:
					if (activeImage.voiManager.getAnnotations() != null) {
						activeImage.annotationsTwisted = new VOI(activeImage.voiManager.getAnnotations());
					} else {
						activeImage.annotationsTwisted = null;
					}
					activeImage.latticeTwisted = new VOIVector(activeImage.voiManager.getLattice());
					
					

					if (activeImage.previewHS != null) {
						for ( int i = 0; i < activeImage.previewHS.length; i++ ) {
							activeImage.previewHS[i].GetImage().disposeLocal(false);
						}
					}
					activeImage.previewHS = untwistingTest();
					for ( int i = 0; i < activeImage.previewHS.length; i++ ) {
						activeImage.previewHS[i].UpdateImages( activeImage.hyperstack[i].GetLUT());
					}
					activeImage.voiManager.removeListeners();
					activeImage.voiManager.setImage(activeImage.previewHS[0].GetImage(), null);
					activeImage.volumeImage = activeImage.previewHS[0];
					activeRenderer.setHyperStack(activeImage.previewHS, activeImage.colormap);
				} 
				else {
					previewCount--;
					previewUntwisting.setText("preview");

					activeImage.voiManager.removeListeners();
					activeImage.voiManager.setImage(activeImage.hyperstack[0].GetImage(), null);
					activeImage.volumeImage = activeImage.hyperstack[0];

					

					// restore twisted annotations and lattice:
					activeImage.wormImage.unregisterAllVOIs();
					VOIVector newLatticeTwisted = activeImage.voiManager.retwistLattice(activeImage.latticeTwisted);
					VOI annotations = activeImage.voiManager.retwistAnnotations(activeImage.latticeTwisted);
					if (newLatticeTwisted != null) {
						activeImage.latticeTwisted = newLatticeTwisted;
					}
					if (annotations != null) {
						for (int i = 0; i < annotations.getCurves().size(); i++) {
							VOIWormAnnotation textRT = (VOIWormAnnotation) annotations.getCurves().elementAt(i);
							boolean newAnnotation = true;
							for (int j = 0; j < activeImage.annotationsTwisted.getCurves().size(); j++) {
								VOIWormAnnotation textT = (VOIWormAnnotation) activeImage.annotationsTwisted.getCurves()
										.elementAt(j);
								if (textT.getText().equals(textRT.getText())) {
									System.err.println(textT.getText() + "   " + textT.firstElement() + "  =>   "
											+ textRT.firstElement());
									if (textRT.modified()) {
										textT.firstElement().copy(textRT.firstElement());
										textT.lastElement().copy(textRT.lastElement());
									}
									newAnnotation = false;
									break;
								}
							}
							if (newAnnotation) {
								// add any new annotations:
								System.err.println("New annotation " + textRT.getText());
								activeImage.annotationsTwisted.getCurves().add(new VOIWormAnnotation(textRT));
							}
						}
					}
					if (activeImage.annotationsTwisted != null) {
						for (int i = activeImage.annotationsTwisted.getCurves().size() - 1; i >= 0; i--) {
							VOIWormAnnotation textT = (VOIWormAnnotation) activeImage.annotationsTwisted.getCurves()
									.elementAt(i);
							boolean deleteAnnotation = true;
							for (int j = 0; j < annotations.getCurves().size(); j++) {
								VOIWormAnnotation textRT = (VOIWormAnnotation) annotations.getCurves().elementAt(j);
								if (textT.getText().equals(textRT.getText())) {
									deleteAnnotation = false;
									break;
								}
							}
							if (deleteAnnotation) {
								activeImage.annotationsTwisted.getCurves().remove(i);
							}
						}
					}
					
					
					
					if (activeImage.previewHS != null) {
						for ( int i = 0; i < activeImage.previewHS.length; i++ ) {
							activeImage.previewHS[i].GetImage().disposeLocal(false);
						}
						activeImage.previewHS = null;
					}
					activeRenderer.setHyperStack(activeImage.hyperstack, activeImage.colormap);
					
					
//					activeImage.voiManager.setImage(activeImage.wormImage, null);
//					activeImage.volumeImage.UpdateData(activeImage.wormImage, activeImage.volumeImage.GetLUT(), true);
//					activeRenderer.resetAxis();
//					activeRenderer.reCreateScene(activeImage.volumeImage);
//
//					// reset image orientation:
//					activeRenderer.SetSceneRotation(activeImage.volumeMatrix);
//
//					updateClipPanel(activeImage, activeRenderer, true);
//					updateHistoLUTPanels(activeImage);
//					if (!activeImage.wormImage.isColorImage()) {
//						((ModelLUT) activeImage.volumeImage.getLUT()).setTransferFunction(fn);
//					} else {
//						((ModelRGB) activeImage.volumeImage.getLUT()).setRedFunction(fn);
//						((ModelRGB) activeImage.volumeImage.getLUT()).setBlueFunction(blueFn);
//						((ModelRGB) activeImage.volumeImage.getLUT()).setGreenFunction(greenFn);
//					}
//					activeImage.volumeImage.UpdateImages(activeImage.volumeImage.getLUT());
//
//					activeImage.voiManager.setPreviewMode(false, activeImage.latticeTwisted,
//							activeImage.annotationsTwisted);
//
//					initDisplayLatticePanel(activeRenderer, activeImage.voiManager, activeImage);
//					if (activeImage.annotationOpen) {
//						initDisplayAnnotationsPanel(activeRenderer, activeImage.voiManager, activeImage);
//					}
//					activeImage.annotationPanelUI.setPreviewMode(false);
//					activeImage.latticeTable.setPreviewMode(false);
				}
				tabbedPane.setSelectedIndex(0);
				tabbedPane.setSelectedIndex(selectedTab);
			}
		}
		if (includeRange != null) {
			int nextStep = dualGPU == null ? 1 : 2;
			imageIndex = Math.min(includeRange.size() - 1, imageIndex);
			imageIndex = Math.max(0, imageIndex);

			if (previewCount == 0) {
				backButton.setEnabled(imageIndex > 0);
				nextButton.setEnabled(imageIndex < (includeRange.size() - nextStep));
				doneButton.setEnabled(true);
			} else {
				doneButton.setEnabled(false);
				backButton.setEnabled(false);
				nextButton.setEnabled(false);
			}
		}
		if (command.equals("close")) {
			setVisible(false);
			if (ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
					&& ViewUserInterface.getReference().isPlugInFrameVisible()) {
				System.exit(0);
			} else {
				dispose();
			}
		}
		if (command.equals("Padding")) {
			try {
				paddingFactor = Integer.valueOf(segmentationPaddingText.getText().trim());
			} catch (NumberFormatException e) {
				paddingFactor = 0;
			}
			if (activeImage.voiManager != null) {
				activeImage.voiManager.setPaddingFactor(paddingFactor);
			}
		} 
		
		if ( source instanceof JCheckBox ) {
			boolean selected = ((JCheckBox)source).isSelected();
			int which =  imageChannel(command);
//			System.err.println(which + "  " + selected );
			if ( which != -1 ) {
				activeRenderer.setImageOn(which, selected);	
			}
		}
	}

	private void checkAnnotations() {
		for (int image = 0; image < imageStack.length; image++) {
			System.err.println(imageStack[image].wormImage.getImageName());
			VOIVector vois = imageStack[image].wormImage.getVOIs();
			for (int v = 0; v < vois.size(); v++) {
				VOI voi = vois.elementAt(v);

				if (voi.getName().equals("annotationVOIs")) {
					System.err.println("     " + voi.getName());
					for (int i = 0; i < voi.getCurves().size(); i++) {
						VOIWormAnnotation text = (VOIWormAnnotation) voi.getCurves().elementAt(i);
						System.err.println("             " + text.getText() + "  " + text.elementAt(0));
					}
				}
			}
		}
	}

	
	private String resultsDir(String sharedDir, ModelImage image) {
		String imageName = image.getImageFileName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		if (imageName.contains("_straight")) {
			imageName = imageName.replaceAll("_straight", "");
		}
		String outputDir = sharedDir + File.separator + JDialogBase.makeImageName(imageName, "") + 
				File.separator + JDialogBase.makeImageName(imageName, "_results") + File.separator;
		return outputDir;
	}
	
	public void rendererConfigured(VolumeTriPlanarRenderBase renderer) {
		
		if ((annotationList != null) && (annotationNames != null) && (triVolume != null)) {
			triVolume.addVOIS(annotationList, annotationNames);
			triVolume.displayAnnotationSpheres();
			triVolume.display3DWindowOnly();
			return;
		}

		if (leftRenderer == renderer) {
			activeRenderer = leftRenderer;
			activeImage = leftImage;
			leftDisplayPanel.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName(), Color.red));
			if (rightDisplayPanel != null) {
				rightDisplayPanel.setBorder(JDialogBase.buildTitledBorder(rightImage.wormImage.getImageName()));
			}
		} else if (rightRenderer != null && rightRenderer == renderer) {
			activeRenderer = rightRenderer;
			activeImage = rightImage;
			leftDisplayPanel.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName()));
			rightDisplayPanel.setBorder(JDialogBase.buildTitledBorder(rightImage.wormImage.getImageName(), Color.red));
		}

		if (activeImage.voiManager == null) {
			activeImage.voiManager = new VOILatticeManagerInterface(null, activeRenderer.getVolumeImage().GetImage(),
					null, 0, true, null);
		}
		activeRenderer.setVOILatticeManager(activeImage.voiManager);

		if ( activeImage.previewHS != null ) {
			activeImage.voiManager.setPreviewMode(true, activeImage.voiManager.getLatticeStraight(),
					activeImage.voiManager.getAnnotationsStraight());
			activeRenderer.resetAxisXInv();
		}
		else if ( activeImage.latticeTwisted != null ) {
			activeImage.voiManager.setPreviewMode(false, activeImage.latticeTwisted,
					activeImage.annotationsTwisted);

			activeRenderer.SetSceneRotation(activeImage.volumeMatrix);
		}
		else {
			if (editMode == ReviewResults) {			
				activeImage.voiManager.setLattice(WormData.readStraightLattice( resultsDir(latticeFileDir, activeImage.wormImage)));
			} 
			else {
				if (loadLegacyLatticeCheck.isSelected()) {
					activeImage.wormImage.setResolutions(originalResolutions);
				}
				activeImage.voiManager
				.setLattice(WormData.readFinalLattice(resultsDir(latticeFileDir, activeImage.wormImage), 
						loadLegacyLatticeCheck.isSelected(), activeImage.wormImage));

				if (loadLegacyLatticeCheck.isSelected()) {
					activeImage.wormImage.setResolutions(new float[] { 1f, 1f, 1 });
				}
			}

			if (activeImage.annotations != null) {
				if (activeImage.annotations.size() > 0) {
					activeImage.voiManager.addAnnotations(activeImage.annotations);
				}
				activeImage.annotations = null;
			}
			VOI annotations = activeImage.voiManager.getAnnotations();
			//		System.err.println( activeImage.wormImage.getImageName() + "  " + annotations);

			if (annotations != null) {
				for (int i = 0; i < annotations.getCurves().size(); i++) {
					final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
					//				System.err.println( "       " + i + "  " + text.getText() + "  " + text.elementAt(0));
					text.createVolumeVOI(activeImage.volumeImage, activeRenderer.getTranslate());
				}
			}
		}
		
		activeRenderer.displayVOIs(true);
		activeImage.voiManager.editAnnotations(editMode == EditSeamCells);
		activeImage.voiManager.colorAnnotations(editMode == EditSeamCells);
		// initialize the display panel for editing / displaying annotations:
		if (editMode == IntegratedEditing) {
			initDisplayCurvesPanel(activeRenderer, activeImage.voiManager, activeImage);
			initDisplayLatticePanel(activeRenderer, activeImage.voiManager, activeImage);
			initDisplayAnnotationsPanel(activeRenderer, activeImage.voiManager, activeImage);

			activeImage.annotationPanelUI.setPreviewMode(activeImage.previewHS != null);
			activeImage.latticeTable.setPreviewMode(activeImage.previewHS != null);
			
			activeImage.voiManager.editAnnotations(false);
			// initDisplaySeamPanel();
		} else if (editMode == EditLattice) {
			initDisplayLatticePanel(activeRenderer, activeImage.voiManager, activeImage);
			activeImage.latticeTable.setPreviewMode(activeImage.previewHS != null);

			activeImage.voiManager.editLattice();
		}

		renderer.displayVolumeSlices(false);
		renderer.displayVolumeRaycast(true);
		renderer.displayVOIs(true);
		renderer.setVolumeBlend(.8f);
		renderer.setABBlend(.8f);
		if (editMode == ReviewResults) {
			renderer.resetAxisXInv();
		}


		initImageChannels(activeImage);
		updateHistoLUTPanels(activeImage);
		updateClipPanel(activeImage, activeRenderer, true);
		updateSurfacePanels();

		if (activeImage.currentTab != -1) {
			tabbedPane.setSelectedIndex(activeImage.currentTab);
		}
		
		int nextStep = dualGPU == null ? 1 : 2;
		imageIndex = Math.min(includeRange.size() - 1, imageIndex);
		imageIndex = Math.max(0, imageIndex);

		if (previewCount == 0) {
			backButton.setEnabled(imageIndex > 0);
			nextButton.setEnabled(imageIndex < (includeRange.size() - nextStep));
			doneButton.setEnabled(true);
		} else {
			doneButton.setEnabled(false);
			backButton.setEnabled(false);
			nextButton.setEnabled(false);
		}
		

		setExtendedState(JFrame.MAXIMIZED_BOTH);
		if (dualGPU != null) {
			dualGPU.setDividerLocation(0.5);
			if (leftImage != null && leftImage.annotationPanelUI != null)
				leftImage.annotationPanelUI.configureListPanel();
			if (rightImage != null && rightImage.annotationPanelUI != null)
				rightImage.annotationPanelUI.configureListPanel();
			if (activeImage != null && activeImage.annotationPanelUI != null)
				activeImage.annotationPanelUI.configureListPanel();
		} else {
			integratedPanel.setDividerLocation(0.5);
			if (activeImage != null && activeImage.annotationPanelUI != null)
				activeImage.annotationPanelUI.configureListPanel();
		}
	}

	public void setActiveRenderer(VolumeTriPlanarRenderBase renderer) {
//		System.err.println("setActiveRenderer");

		VolumeTriPlanarRenderBase previousActive = activeRenderer;
		if (leftRenderer == renderer) {
			activeRenderer = leftRenderer;
			leftDisplayPanel.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName(), Color.red));
			if (rightDisplayPanel != null)
				rightDisplayPanel.setBorder(JDialogBase.buildTitledBorder(rightImage.wormImage.getImageName()));
			activeImage = leftImage;
		} else if (rightRenderer != null && rightRenderer == renderer) {
			activeRenderer = rightRenderer;
			leftDisplayPanel.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName()));
			rightDisplayPanel.setBorder(JDialogBase.buildTitledBorder(rightImage.wormImage.getImageName(), Color.red));
			activeImage = rightImage;
		}

				
		if (previousActive != activeRenderer) {

			if (activeImage.voiManager.isPreview()) {
				previewUntwisting.setText("return");
			} else {
				previewUntwisting.setText("preview");
			}
			displayModel.setSelected(activeImage.voiManager.isModelDisplayed());
			displaySurface.setSelected(activeRenderer.getSurface("worm") != null);

			updateSurfacePanels();
			updateClipPanel(activeImage, activeRenderer, true);
			updateHistoLUTPanels(activeImage);

			if (editMode == EditLattice) {
				leftImage.voiManager.editLattice();
				rightImage.voiManager.editLattice();
			}
			if (activeImage.currentTab != -1) {
				tabbedPane.setSelectedIndex(activeImage.currentTab);
				stateChanged(null);
			}
		}
		int nextStep = dualGPU == null ? 1 : 2;
		if (includeRange != null) {
			imageIndex = Math.min(includeRange.size() - 1, imageIndex);
			imageIndex = Math.max(0, imageIndex);
		}

		if (previewCount == 0 && (includeRange != null)) {
			backButton.setEnabled(imageIndex > 0);
			nextButton.setEnabled(imageIndex < (includeRange.size() - nextStep));
			doneButton.setEnabled(true);
		} else {
			doneButton.setEnabled(false);
			backButton.setEnabled(false);
			nextButton.setEnabled(false);
		}
	}

	public void dispose() {
		super.dispose();
		closeAll();
	}

	/**
	 * Called when the user is done viewing the volumes and editing the seam cells,
	 * lattice, or annotations. The next step in the straightening process is
	 * automatically enabled and selected.
	 * 
	 * @param mode
	 */
	public void enableNext(int mode) {
		closeAll();

		if (mode == EditLattice) {
			checkSeamCells.setSelected(true);
		} else if (mode == CheckSeam) {
			integratedEdit.setSelected(true);
		} else if (mode == IntegratedEditing) {
			latticeStraighten.setSelected(true);
		} else if (mode == ReviewResults) {
			calcMaxProjection.setSelected(true);
		}
		imageIndex = 0;

		editMode = EditNONE;
		batchProgress.setValue(0);
		batchProgress.update(batchProgress.getGraphics());
		volumePanel.setVisible(false);
		lutPanel.removeAll();
		opacityPanel.removeAll();
		clipPanel.removeAll();
		tabbedPane.addTab("LUT", null, lutPanel);
		tabbedPane.addTab("Opacity", null, opacityPanel);
		tabbedPane.addTab("Clip", null, clipPanel);
		tabbedPane.addChangeListener(this);

		startButton.setEnabled(true);
		if (latticeSelectionPanel != null) {
			latticeSelectionPanel.setVisible(false);
		}
//		System.err.println(currentSize);
		setExtendedState(JFrame.NORMAL);
		pack();
		setPreferredSize(currentSize);
		setSize(currentSize);
		pack();
	}

	private void closeAll() {
		tabbedPane.setVisible(false);
		tabbedPane.removeChangeListener(this);
		tabbedPane.removeAll();
		activeRenderer = null;
		activeImage = null;
		gpuPanel.removeAll();

		lightsPanel = null;
		annotationPanels = null;
		latticePanel = null;
		curvePanel = null;

		if (leftRenderer != null) {
			leftRenderer.dispose();
			leftRenderer = null;
		}

		if (rightRenderer != null) {
			rightRenderer.dispose();
			rightRenderer = null;
		}

		if (imageStack != null) {
			for (int i = 0; i < imageStack.length; i++) {
				if (imageStack[i].volumeImage != null) {
					imageStack[i].volumeImage.dispose();
					imageStack[i].volumeImage = null;
				}
			}
			for (int i = 0; i < imageStack.length; i++) {
				if (imageStack[i].previewImage != null) {
					imageStack[i].previewImage.disposeLocal(false);
					imageStack[i].previewImage = null;
				}
				if (imageStack[i].wormImage != null) {
					imageStack[i].wormImage.disposeLocal(false);
					imageStack[i].wormImage = null;
				}
				if (imageStack[i].contourImage != null) {
					imageStack[i].contourImage.disposeLocal(false);
					imageStack[i].contourImage = null;
				}
				if (imageStack[i].voiManager != null) {
					imageStack[i].voiManager = null;
				}
				if ( imageStack[i].hyperstack != null ) {
					for ( int j = 0; j < imageStack[i].hyperstack.length; j++ ) {
						if ( imageStack[i].hyperstack[j] != null ) {
							imageStack[i].hyperstack[j].dispose();
							imageStack[i].hyperstack[j] = null;							
						}
					}
				}
			}
			imageStack = null;
		}

		if (includeRange != null) {
			includeRange.clear();
			includeRange = null;
		}
		if (triVolume != null) {
			triVolume.disposeLocal(false);
			triVolume = null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.
	 * PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		String propertyName = event.getPropertyName();
		if (propertyName.equals("Opacity")) {
			int which = opacityTab.getSelectedIndex();
//			System.err.println("propertyChange " + which);
			if ( which != -1 ) {
				final TransferFunction kTransfer = activeImage.volOpacityPanel[which].getCompA().getOpacityTransferFunction();
				updateImages(activeImage.colormap, activeImage.hyperstack[which].GetImage(), kTransfer, which);
				activeImage.hyperstack[which].UpdateImages(kTransfer, 0, null);
//
//				if ( activeImage.previewHS != null ) {
//					activeImage.previewHS[which].UpdateImages(kTransfer, 0, null);
//				}
			}
		}
	}

	@Override
	public void setSlice(int slice) {
	}

	@Override
	public void setTimeSlice(int tSlice) {
	}

	@Override
	public boolean updateImageExtents() {
		return false;
	}

	@Override
	public boolean updateImages() {
//		 System.err.println("updateImages" );
		if (activeImage.hyperstack != null && lutTab != null) {
			int which = lutTab.getSelectedIndex();
			if ( which != -1 ) {
				updateImages(activeImage.colormap, activeImage.hyperstack[which].GetLUT(), which );
//				System.err.println("updateImages " + which);
				activeImage.hyperstack[which].UpdateImages(activeImage.hyperstack[which].getLUT());
//				if ( activeImage.previewHS != null ) {
//					activeImage.previewHS[which].UpdateImages(activeImage.hyperstack[which].getLUT());
//				}
//				if ((activeRenderer != null) && (activeImage.hyperstack[which].getLUT() instanceof ModelRGB)) {
//					activeRenderer.setRGBTA((ModelRGB) activeImage.hyperstack[which].getLUT());
//				}
			}
		}
		return false;
	}
	
	private void updateImages(Texture texture, ModelLUT lut, int index ) {
		byte[] data = texture.GetImage().GetData();
		ModelLUT.exportIndexedLUTMin(lut, data, index);
		texture.Reload(true);
	}

	private void updateImages(Texture texture, ModelImage image, TransferFunction tf, int index ) {
		final int lutHeight = texture.GetImage().GetBound(0);
		final byte[] data = texture.GetImage().GetData();
		int offset = index * lutHeight * 4;

		final float fRange = (float) (image.getMax() - image.getMin());
		final float fStep = fRange / lutHeight;
		float fDataValue = (float) image.getMin();
		float fVal;
		for (int i = 0; i < lutHeight; i++) {
			fVal = (tf.getRemappedValue(fDataValue, lutHeight) / 255.0f);
			data[offset + i * 4 + 3] = (byte) (fVal * 255);
			fDataValue += fStep;
		}
		texture.Reload(true);
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
	 */
	public boolean updateImages(boolean flag) {
		return updateImages();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(gov.nih.mipav.model.
	 * structures.ModelLUT, gov.nih.mipav.model.structures.ModelLUT, boolean, int)
	 */
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
		System.err.println("updateImages " + flag + "  " + interpMode);
//		if (activeImage.volumeImage != null) {
//			activeImage.volumeImage.UpdateImages(LUTa);
//		}
		return false;
	}

	@Override
	public void windowActivated(WindowEvent e) {
	}

	@Override
	public void windowClosed(WindowEvent e) {
	}

	public void windowClosing(final WindowEvent event) {
		if (editMode == IntegratedEditing || editMode == EditLattice) {
			int result = JOptionPane.showConfirmDialog(null, "Save changes before closing?", "Save",
					JOptionPane.YES_NO_OPTION);
			if (result == JOptionPane.YES_OPTION) {
				saveAll();
			}
		}

		if (ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
				&& ViewUserInterface.getReference().isPlugInFrameVisible()) {
			System.exit(0);
		} else {
			dispose();
		}
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
	}

	@Override
	public void windowIconified(WindowEvent e) {
	}

	@Override
	public void windowOpened(WindowEvent e) {
	}

	/**
	 * Opens the current image and annotation VOIs for viewing/editing. protected
	 * void openAnnotations( int whichImage ) { if ( includeRange != null ) { if (
	 * (imageIndex >= 0) && (imageIndex < includeRange.size()) ) {
	 * backNextPanel.remove(displayModel); backNextPanel.add(displayModel);
	 * displayModel.setSelected(false); backNextPanel.remove(displaySurface);
	 * backNextPanel.add(displaySurface); displaySurface.setSelected(false);
	 * backNextPanel.remove(previewUntwisting);
	 * backNextPanel.add(previewUntwisting);
	 * 
	 * String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) +
	 * ".tif"; File voiFile = new File(baseFileDir + File.separator + fileName);
	 * File voiFile2 = new File(baseFileDir2 + File.separator + fileName); if (
	 * openImages( voiFile, null, fileName ) ) { // open default file and get the
	 * lattice: wormData = new WormData(wormImage); finalLattice =
	 * wormData.readFinalLattice(); if ( whichImage == 0 ) { // continue with the
	 * default file: if ( finalLattice != null ) { wormImage.registerVOI(
	 * finalLattice ); }
	 * 
	 * if ( annotations != null ) { annotations.clear(); annotations = null; }
	 * annotations = new VOIVector(); VOI markers = wormData.getMarkerAnnotations();
	 * if ( markers != null ) { annotations.add( markers ); wormImage.registerVOI(
	 * markers ); } if ( (annotations.size() > 0) && (voiManager != null) ) {
	 * voiManager.setAnnotations(annotations); initDisplayAnnotationsPanel();
	 * 
	 * voiManager.editAnnotations(false); VOIVector latticeVector = new VOIVector();
	 * latticeVector.add(finalLattice); voiManager.setLattice(latticeVector); } }
	 * else { // open the second image and set the lattice and markers: if (
	 * openImages( voiFile2, null, fileName ) ) { wormData = new
	 * WormData(wormImage); wormImage.registerVOI( finalLattice );
	 * 
	 * if ( annotations != null ) { annotations.clear(); annotations = null; }
	 * annotations = new VOIVector(); VOI markers = wormData.getMarkerAnnotations();
	 * if ( markers != null ) { annotations.add( markers ); wormImage.registerVOI(
	 * markers ); } if ( (annotations.size() > 0) && (voiManager != null) ) {
	 * initDisplayAnnotationsPanel();
	 * 
	 * voiManager.setAnnotations(annotations); voiManager.editAnnotations(false);
	 * VOIVector latticeVector = new VOIVector(); latticeVector.add(finalLattice);
	 * voiManager.setLattice(latticeVector); } } } } else { imageIndex +=
	 * nextDirection; openAnnotations(whichImage); } } } }
	 */

	private List<String> readProcessOutput(InputStream inputStream) throws IOException {
		try (BufferedReader output = new BufferedReader(new InputStreamReader(inputStream))) {
			return output.lines().collect(Collectors.toList());
		}
	}

	public boolean trackMIPAV_Python() throws Exception {

		// System.err.println( "python" + baseFileDir + File.separator +
		// "track_MIPAV.py" + " " +
		// leftImage.wormData.getStraightAnnotationsPath() + " " +
		// rightImage.wormData.getStraightAnnotationsPath() + " " +
		// leftImage.wormData.getAnnotationsPath() + " " +
		// rightImage.wormData.getAnnotationsPath() );

		ProcessBuilder processBuilder = new ProcessBuilder("python", baseFileDir + File.separator + "track_MIPAV.py",
				WormData.getStraightAnnotationsPath(resultsDir(latticeFileDir, leftImage.wormImage)), 
				WormData.getStraightAnnotationsPath(resultsDir(latticeFileDir, rightImage.wormImage)),
				WormData.getIntegratedMarkerAnnotationsPath(resultsDir(latticeFileDir, leftImage.wormImage)),
				WormData.getIntegratedMarkerAnnotationsPath(resultsDir(latticeFileDir, rightImage.wormImage)));
		processBuilder.redirectErrorStream(true);

		Process process = processBuilder.start();
		List<String> results = readProcessOutput(process.getInputStream());
		int exitCode = process.waitFor();
		if (exitCode != 0) {
			String error = "Python error - track_MIPAV:\n";
			for (int i = 0; i < results.size(); i++) {
				error += results.get(i) + "\n";
			}
			MipavUtil.displayError(error);
		}
		return exitCode == 0;
	}

	public boolean restraightenMIPAV_Python(IntegratedWormData data) throws Exception {

		// System.err.println( "python" + baseFileDir + File.separator +
		// "track_MIPAV.py" + " " +
		// leftImage.wormData.getStraightAnnotationsPath() + " " +
		// rightImage.wormData.getStraightAnnotationsPath() + " " +
		// leftImage.wormData.getAnnotationsPath() + " " +
		// rightImage.wormData.getAnnotationsPath() );

		String predictedTwisted = resultsDir(latticeFileDir, data.wormImage) + File.separator + "prediction" + File.separator
				+ "predicted_annotations.csv";
		String predictedStraight = resultsDir(latticeFileDir, data.wormImage) + File.separator + "prediction" + File.separator
				+ "predicted_straightened_annotations.csv";

		File file = new File(predictedTwisted);
		if (!file.exists())
			return true;
		file = new File(predictedStraight);
		if (!file.exists())
			return true;

		ProcessBuilder processBuilder = new ProcessBuilder("python",
				baseFileDir[0] + File.separator + "restraighten_MIPAV.py", predictedStraight, predictedTwisted,
				WormData.getIntegratedMarkerAnnotationsPath( resultsDir(latticeFileDir, rightImage.wormImage)));
		processBuilder.redirectErrorStream(true);

		Process process = processBuilder.start();
		List<String> results = readProcessOutput(process.getInputStream());
		int exitCode = process.waitFor();
		if (exitCode != 0) {
			String error = "Python error - restraighten_MIPAV:\n";
			for (int i = 0; i < results.size(); i++) {
				error += results.get(i) + "\n";
			}
			MipavUtil.displayError(error);
		}
		return exitCode == 0;
	}

	private void runPython() {
		// delete the predicted files for the right image:
		String fileName = resultsDir(latticeFileDir, rightImage.wormImage) + File.separator + "prediction" + File.separator
				+ "predicted_straightened_annotations.csv";
		File straightFile = new File(fileName);
		if (straightFile.exists()) {
			straightFile.delete();
		}
		fileName = resultsDir(latticeFileDir, rightImage.wormImage) + File.separator + "prediction" + File.separator
				+ "predicted_annotations.csv";
		File twistedFile = new File(fileName);
		if (twistedFile.exists()) {
			twistedFile.delete();
		}

		// when user presses 'predict'
		// prototype:
		// 1. straighten the right image - left annotations remain fixed:
		ModelImage contourImage = rightImage.voiManager.untwistAnnotations(resultsDir(latticeFileDir, rightImage.wormImage), rightImage.contourImage);
		if (rightImage.contourImage != null && rightImage.contourImage != contourImage) {
			rightImage.contourImage.disposeLocal(false);
			rightImage.contourImage = null;
		}
		rightImage.contourImage = contourImage;

		// 2. call track_MIPAV.py on the four straightened images
		try {
			System.err.println("Calling Python?");
//			if ( !changed ) {
//				restraightenMIPAV_Python(rightImage);
//			}
			if (trackMIPAV_Python()) {
				// wait for file:
				int count = 100;
				while (count > 0 && !twistedFile.exists()) {
					try {
						System.err.println("file doesn't exist yet:");
						wait(100);
						count--;
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				if (twistedFile.exists()) {
					System.err.println("file exists length = " + twistedFile.length());
					// 3. delete annotations from right image & load predictions
					loadPredicted(rightImage);
				} else {
					MipavUtil.displayError("prediction failed");
				}
			} else {
				MipavUtil.displayError("prediction failed");
			}
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/**
	 * Untwists the worm image quickly for the preview mode - without saving any
	 * images or statistics
	 * 
	 * @return untwisted image.
	 */
	private VolumeImage[] untwistingTest() {
		activeImage.voiManager.setPaddingFactor(paddingFactor);
		ModelImage[] images = activeImage.voiManager.untwistTest(activeImage.hyperstack);
		VolumeImage[] hyperstack = new VolumeImage[images.length];
		for ( int i = 0; i < images.length; i++ ) {
			images[i].unregisterAllVOIs();
			hyperstack[i] = new VolumeImage(false, images[i], "" + i, null, 0, false);
		}
		return hyperstack;
	}

	/**
	 * Checks image size and the available memory on the GPU.
	 * 
	 * @param sizeA
	 * @param sizeB
	 * @return
	 */
	private boolean checkGPUMemory(ModelImage[] images) {
		long[] maxMemSizeArray = new long[2];
		OpenCLInfo.getMaxMemSize(CL_DEVICE_TYPE_GPU, maxMemSizeArray);
		long maxAllocSize = maxMemSizeArray[0];
		long totalMemSize = maxMemSizeArray[1];
		
		long memoryUsed = 0;
		for ( int i = 0; i < images.length; i++ ) {
			if ( images[i] != null ) {
				int dataSize = images[i].getDataSize();
				if ( dataSize > (maxAllocSize / (Sizeof.cl_float)) ) {
					return false;
				}
				memoryUsed += dataSize;
			}
		}

		if ( memoryUsed >= (totalMemSize / Sizeof.cl_float) ) {
			return false;
		}
		return true;
	}

	/**
	 * Opens the current volume for viewing including the straightened annotations
	 * and lattice.
	 */
	private boolean openHyperStack() {
		boolean success = false;
		if (includeRange != null) {
			// If images are opened as a hyperstack, open all images and save in an array:
			if (imageStack == null) {
				// count images to make sure all exist:
				for (int i = includeRange.size() - 1; i >= 0; i--) {
					String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
					File voiFile = new File(baseFileDir[0] + File.separator + fileName);
					if (editMode == ReviewResults) {
						fileName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
						String subDirName = baseFileName + "_" + includeRange.elementAt(i) + File.separator;
						String subDirNameResults = baseFileName + "_" + includeRange.elementAt(i) + "_results"
								+ File.separator;
						voiFile = new File(baseFileDir[0] + File.separator + subDirName + subDirNameResults
								+ PlugInAlgorithmWormUntwistingJanelia.outputImages + File.separator + fileName);
					}
					if (!voiFile.exists()) {
						includeRange.remove(i);
					}
				}
				if (includeRange.size() == 0) {
					MipavUtil.displayError("No images available, check file path");
					return false;
				}

				imageStack = new IntegratedWormData[includeRange.size()];

				ViewJProgressBar progressBar = new ViewJProgressBar("Opening HyperStack...", "Opening HyperStack...", 0,
						includeRange.size(), false, null, null);
				MipavUtil.centerOnScreen(progressBar);
				progressBar.setVisible(true);
				progressBar.updateValueImmed(0);

				// check memory usage:
				System.gc();
				long memoryInUse = MipavUtil.getUsedHeapMemory();

				for (int i = 0; i < includeRange.size(); i++) {
					
					ModelImage[] images = new ModelImage[baseFileDir.length];

					String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
					File[] imageFiles = new File[baseFileDir.length];
					for ( int j = 0; j < baseFileDir.length; j++ ) {
						if (editMode == ReviewResults) {
							fileName = baseFileName + "_" + includeRange.elementAt(i) + "_straight.tif";
							String subDirName = baseFileName + "_" + includeRange.elementAt(i) + File.separator;
							String subDirNameResults = baseFileName + "_" + includeRange.elementAt(i) + "_results"
									+ File.separator;
							imageFiles[j] = new File(baseFileDir[j] + File.separator + subDirName + subDirNameResults
									+ PlugInAlgorithmWormUntwistingJanelia.outputImages + File.separator + fileName);
						}
						else {
							imageFiles[j] = new File(baseFileDir[j] + File.separator + fileName);
						}

						if (imageFiles[j].exists()) {
							images[j] = openImage(imageFiles[j], fileName);
							System.err.println("Opening... " + fileName + "  " + images[j].isColorImage() );
							images[j].calcMinMax();
							if ( images[j].isColorImage() ) {
								images[j] = convertToGray(images[j]);
							}
						}
					}

					// Add memory check here:
					if (i == 0) {
						if ( !checkGPUMemory(images) ) {
							MipavUtil.displayError("Image size too big to load on GPU.");
							progressBar.setVisible(false);
							progressBar.dispose();
							progressBar = null;
							for ( int j = 0; j < images.length; j++ ) {
								if (images[j] != null) {
									images[j].disposeLocal();
									images[j] = null;
								}
							}
							return false;
						}

						System.gc();
						long memoryInUse2 = MipavUtil.getUsedHeapMemory();
						long imagesMemory = memoryInUse2 - memoryInUse;

						final long totalMemory = MipavUtil.getMaxHeapMemory();
						final long memoryFree = totalMemory - memoryInUse2;
						System.err.println("image memory use: " + (imagesMemory / 1048576) + "M");
						if ((imagesMemory * includeRange.size()) > memoryFree) {
							MipavUtil.displayError("Too many images, please load shorter sequence");
							progressBar.setVisible(false);
							progressBar.dispose();
							progressBar = null;
							for ( int j = 0; j < images.length; j++ ) {
								if (images[j] != null) {
									images[j].disposeLocal();
									images[j] = null;
								}
							}
							return false;
						}
					}

					leftImage = new IntegratedWormData();

					thresholdImage(images);
					leftImage.wormImage = images[0];
					
					imageStack[i] = leftImage;
					System.err.println("... adding " + i + " " + leftImage.wormImage.getImageName());

					leftImage.wormImage.setImageName(leftImage.wormImage.getImageName().replace("_rgb", ""));

					if (leftImage.annotations != null) {
						leftImage.annotations.clear();
						leftImage.annotations = null;
					}
					leftImage.hyperstack = new VolumeImage[images.length];
					for ( int j = 0; j < images.length; j++ ) {
						leftImage.hyperstack[j] = new VolumeImage(false, images[j], "" + j, null, 0, false);
					}
					leftImage.volumeImage = leftImage.hyperstack[0]; //new VolumeImage(false, leftImage.wormImage, "A", null, 0, false);
					leftImage.voiManager = new VOILatticeManagerInterface(null, leftImage.volumeImage.GetImage(), null,
							0, true, null);
					
					byte[] aucData = new byte[256 * 4 * images.length];
					GraphicsImage cmImage = new GraphicsImage(GraphicsImage.FormatMode.IT_RGBA8888, 256, images.length, aucData, new String("colormap" + leftImage.volumeImage.GetImage().getImageFileName()));
					leftImage.colormap = new Texture();
					leftImage.colormap.SetImage(cmImage);

					openAnnotations(leftImage);
					openNeuriteCurves(leftImage);

					initHistoLUTPanel(leftImage);
					progressBar.updateValueImmed(i);
				}

				progressBar.setVisible(false);
				progressBar.dispose();
				progressBar = null;

				leftImage = imageStack[imageIndex];
				if (leftImage.wormImage == null)
					return false;
				success = true;

				leftRenderer = new VolumeTriPlanarRender( leftImage.hyperstack, leftImage.colormap );
				leftRenderer.setVisible(false);
				leftRenderer.addConfiguredListener(this);

				if ((imageIndex + 1) < imageStack.length) {
					rightImage = imageStack[imageIndex + 1];

					rightRenderer = new VolumeTriPlanarRender(rightImage.hyperstack, rightImage.colormap);
//					rightRenderer = new VolumeTriPlanarRender(null, rightImage.volumeImage, new VolumeImage());
					rightRenderer.addConfiguredListener(this);
					rightRenderer.setVisible(false);

					leftDisplayPanel = new JPanel(new BorderLayout());
					leftDisplayPanel.add(leftRenderer.GetCanvas(), BorderLayout.CENTER);
					leftDisplayPanel
							.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName(), Color.red));

					rightDisplayPanel = new JPanel(new BorderLayout());
					rightDisplayPanel.add(rightRenderer.GetCanvas(), BorderLayout.CENTER);
					rightDisplayPanel.setBorder(JDialogBase.buildTitledBorder(rightImage.wormImage.getImageName()));

					dualGPU = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftDisplayPanel, rightDisplayPanel);
					dualGPU.setOneTouchExpandable(true);
					dualGPU.setDividerSize(6);
					dualGPU.setContinuousLayout(true);
					dualGPU.setResizeWeight(0.5);
					dualGPU.setDividerLocation(0.5);
					dualGPU.setVisible(true);
					gpuPanel.add(dualGPU, BorderLayout.CENTER);

					// Provide minimum sizes for the two components in the split pane
					Dimension minimumSize = new Dimension(100, 50);
					leftDisplayPanel.setMinimumSize(minimumSize);
					rightDisplayPanel.setMinimumSize(minimumSize);
				} else {
					rightRenderer = null;

					leftDisplayPanel = new JPanel(new BorderLayout());
					leftDisplayPanel.add(leftRenderer.GetCanvas(), BorderLayout.CENTER);
					leftDisplayPanel.setBorder(JDialogBase.buildTitledBorder(leftImage.wormImage.getImageName()));
					gpuPanel.add(leftDisplayPanel, BorderLayout.CENTER);
					Dimension minimumSize = new Dimension(100, 50);
					leftDisplayPanel.setMinimumSize(minimumSize);
				}
				activeImage = leftImage;
				activeRenderer = leftRenderer;

				latticeSelectionPanel.removeAll();
				latticeSelectionPanel.setVisible(false);

				if (editMode == EditLattice) {

					latticeSelectionPanel.add(newLatticeButton);
					latticeSelectionPanel.add(flipLatticeButton);
				}

				if (editMode != ReviewResults) {
					latticeSelectionPanel.add(displayModel);
					displayModel.setSelected(false);
					latticeSelectionPanel.add(displaySurface);
					displaySurface.setSelected(false);
					latticeSelectionPanel.add(editCrossSections);
					editCrossSections.setSelected(false);
					if (editMode == IntegratedEditing && (dualGPU != null)) {
						latticeSelectionPanel.remove(predict);
						latticeSelectionPanel.add(predict);
					}
					latticeSelectionPanel.remove(previewUntwisting);
					latticeSelectionPanel.add(previewUntwisting);
					latticeSelectionPanel.setVisible(true);
				}

				gpuPanel.setVisible(true);
				tabbedPane.setVisible(true);
				volumePanel.setVisible(true);
				pack();
				if (rightRenderer != null) {
					rightRenderer.startAnimator(true);
				}
				leftRenderer.startAnimator(true);

				if (dualGPU != null) {
					dualGPU.setOneTouchExpandable(true);
					dualGPU.setDividerSize(6);
					dualGPU.setContinuousLayout(true);
					dualGPU.setResizeWeight(0.5);
					dualGPU.setDividerLocation(0.5);
					dualGPU.setVisible(true);
				}
			}
			int nextStep = dualGPU == null ? 1 : 2;
			doneButton.setEnabled(true);
			nextButton.setEnabled(imageIndex < (includeRange.size() - nextStep));
			backButton.setEnabled(imageIndex > 0);
		}

		integratedPanel.setDividerLocation(0.25);
		return success;
	}

	private void thresholdImage(ModelImage[] images ) {
		if (thresholdImageCheck.isSelected()) {
			for ( int i = 0; i < images.length; i++ ) {
				if ( images[i] != null ) {

					for (int j = 0; j < images[i].getDataSize(); j++) {
						if (images[i].getFloat(j) > threshold) {
							images[i].set(j, threshold);
						}
					}
					images[i].calcMinMax();
				}
			}
		}
	}

	private ModelImage convertToGray(ModelImage image ) {
		ModelImage result = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(),
				JDialogBase.makeImageName(image.getImageFileName(), ""));
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1; 
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;		
		
		float max = 0;
		float maxSum = 0;
		for ( int z = 0; z < dimZ; z++ ) {
			for ( int y = 0; y < dimY; y++ ) {
				for ( int x = 0; x < dimX; x++ ) {
					float r = image.getFloatC(x, y, z, 1);
					float g = image.getFloatC(x, y, z, 2);
					float b = image.getFloatC(x, y, z, 3);
					if ( r > max ) max = r;
					if ( g > max ) max = g;
					if ( b > max ) max = b;
					float sum = r + b + g;
					if ( sum > maxSum ) maxSum = sum;
					result.set(x, y, z, sum);
				}
			}
		}
		if ( maxSum > max ) {
			float scale = maxSum / max;
			for ( int i = 0; i < result.getDataSize(); i++ ) {
				result.set(i,  result.getFloat(i) / scale);;
			}
		}
		result.calcMinMax();
		return result;
	}
	
	private ModelImage combineImages(ModelImage imageA, ModelImage imageB, ModelImage imageC) {
		// imageA is never null and is always written into the 'green' channel...

		ModelImage displayImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, imageA.getExtents(),
				JDialogBase.makeImageName(imageA.getImageName(), "_rgb"));
		JDialogBase.updateFileInfo(imageA, displayImage);

		// Make algorithm
		ModelImage blank = null;
		if ( imageB == null && imageC != null ) {
			blank = new ModelImage(ModelImage.SHORT, imageA.getExtents(),
					JDialogBase.makeImageName(imageA.getImageName(), ""));

			AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(blank, imageA, imageC, displayImage, true, false, 255,
					true, true);
			mathAlgo.run();
			blank.disposeLocal(false);
		}
		else if ( imageB != null && imageC == null ) {
			blank = new ModelImage(ModelImage.SHORT, imageA.getExtents(),
					JDialogBase.makeImageName(imageA.getImageName(), ""));

			AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(imageB, imageA, blank, displayImage, true, false, 255,
					true, true);
			mathAlgo.run();
			blank.disposeLocal(false);
		}
		else {
			AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(imageB, imageA, imageC, displayImage, true, false, 255,
					true, true);
			mathAlgo.run();
		}

		displayImage.calcMinMax();
		return displayImage;
	}

	private float[] originalResolutions;

	protected ModelImage openImage(File imageFile, String fileName) {

		if (imageFile.exists()) {
			FileIO fileIO = new FileIO();
			ModelImage image = fileIO.readImage(fileName, imageFile.getParent() + File.separator, false, null);
			image.calcMinMax();
			originalResolutions = image.getResolutions(0).clone();
			image.setResolutions(new float[] { 1, 1, 1 });
			return image;
		}
		return null;
	}

	/**
	 * Displays the annotation animation visualization framework.
	 */
	private void annotationAnimationFromSpreadSheet() {
		int[] extents = new int[3];
		VOIVector tempList = new VOIVector();
		Vector<int[]> timesList = new Vector<int[]>();
		ViewJProgressBar progress = new ViewJProgressBar("Generating Animation", "", 0, 100, false);
		MipavUtil.centerOnScreen(progress);

		String inputDirName = baseFileDir[0] + File.separator;
		 System.err.println( inputDirName );
		final File inputFileDir = new File(inputDirName);

		Vector3f min = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE);
		Vector3f max = new Vector3f(-Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE);
		// int timeCount, maxTimeCount = -1, minTimeCount = Integer.MAX_VALUE;
		// int maxIndex = -1;
		int fileIndex = 0;
		int startTime = -1;
		int endTime = -1;
		if (inputFileDir.exists() && inputFileDir.isDirectory()) {
			progress.setVisible(true);
			String[] list = inputFileDir.list();
			for (int i = 0; i < list.length; i++) {
				File annotationFile = new File(inputFileDir + File.separator + list[i]);
				if (annotationFile.isDirectory()) {
					continue;
				}
				if (!list[i].endsWith(".csv")) {
					continue;
				}
				int index = list[i].indexOf(".");
				String annotationName = new String(list[i]);
				if (index != -1) {
					annotationName = annotationName.substring(0, index);
				}
				System.err.println(annotationName);

				// timeCount = 0;
				VOI annotation = new VOI((short) i, annotationName, VOI.ANNOTATION, 0);
				Vector<Integer> times = new Vector<Integer>();
				FileReader fr;
				try {
					int red = 255;
					int green = 255;
					int blue = 255;
					int label = 1;
					fr = new FileReader(annotationFile);
					BufferedReader br = new BufferedReader(fr);

					String line = br.readLine();
					String[] parsed = line.split(",");
					while (line != null) {
						// System.err.println(line);
						int time = -1;
						float x = 0, y = 0, z = 0;
						parsed = line.split(",");
						// time:
						time = (parsed.length > 0) ? (parsed[0].length() > 0) ? Integer.valueOf(parsed[0]) : -1 : -1;
						// position:
						x = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf(parsed[1]) : 0 : 0;
						y = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf(parsed[2]) : 0 : 0;
						z = (parsed.length > 3) ? (parsed[3].length() > 0) ? Float.valueOf(parsed[3]) : 0 : 0;
						// color:
						red = (int) ((parsed.length > 4) ? (parsed[4].length() > 0) ? Float.valueOf(parsed[4]) : red
								: red);
						green = (int) ((parsed.length > 5) ? (parsed[5].length() > 0) ? Float.valueOf(parsed[5]) : green
								: green);
						blue = (int) ((parsed.length > 6) ? (parsed[6].length() > 0) ? Float.valueOf(parsed[6]) : blue
								: blue);
						// show label:
						label = (int) ((parsed.length > 7) ? (parsed[7].length() > 0) ? Float.valueOf(parsed[7]) : label
								: label);

						if (time != -1) {
							if (startTime > time) {
								startTime = time;
							}
							if (endTime < time) {
								endTime = time;
							}
						}
						if ((time != -1) && (z >= 0) && (parsed.length > 3)) {
							VOIText text = new VOIText();
							text.setText(list[i]);
							text.setColor(new Color(red, green, blue));
							text.add(new Vector3f(x, y, z));
							text.add(new Vector3f(x, y, z));
							text.setText(annotationName);
							text.display(label == 1);
							annotation.getCurves().add(text);

							times.add(time);
							// timeCount++;

							min.min(text.elementAt(0));
							max.max(text.elementAt(0));

//							System.err.println( annotationName + "  " + time + "   " + x + "  " + y + "  " + z );
							// if ( text.elementAt(0).Z < 0 )
							// {
							// System.err.println(list[i] );
							// }
						}
						line = br.readLine();
					}
					br.close();
					fr.close();

					tempList.add(annotation);

					int[] timesArray = new int[times.size()];
					for (int j = 0; j < times.size(); j++) {
						timesArray[j] = times.elementAt(j);
					}
					timesList.add(timesArray);
				} catch (FileNotFoundException e) {
					MipavUtil.displayError("Error reading file: " + inputDirName + " " + e.getCause());
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					MipavUtil.displayError("Error reading file: " + inputDirName + " " + e.getCause());
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				// if ( timeCount < minTimeCount )
				// {
				// minTimeCount = timeCount;
				// maxIndex = fileIndex;
				// }
				// if ( timeCount > maxTimeCount )
				// {
				// maxTimeCount = timeCount;
				// maxIndex = fileIndex;
				// }
				fileIndex++;
				progress.updateValueImmed((int) (100 * (float) fileIndex / (float) list.length));
			}
		}

		// System.err.println( minTimeCount + " " + maxTimeCount + " " + (minTimeCount
		// == maxTimeCount ) );

		// System.err.println( timesList.size() + " " + tempList.size() );
		// int[] times = timesList.elementAt( maxIndex );
		// VOI curve = tempList.elementAt( maxIndex );
		// for ( int j = 0; j < times.length; j++ )
		// {
		// System.err.println( curve.getName() + " " + times[j] );
		// }

		annotationList = new VOIVector();
		for (int i = startTime; i <= endTime; i++) {
			// int timeStep = times[i];
			VOI annotation = new VOI((short) i, "time" + i, VOI.ANNOTATION, 0);

			// System.err.print( timeStep );
			for (int j = 0; j < timesList.size(); j++) {
				int[] currentTimes = timesList.elementAt(j);
				for (int k = 0; k < currentTimes.length; k++) {
					if (i == currentTimes[k]) {
						VOIText text = new VOIText(tempList.elementAt(j).getCurves().elementAt(k));

						text.setText(((VOIText) tempList.elementAt(j).getCurves().elementAt(k)).getText());
						text.setColor(((VOIText) tempList.elementAt(j).getCurves().elementAt(k)).getColor());
						text.display(((VOIText) tempList.elementAt(j).getCurves().elementAt(k)).getDisplay());
						annotation.getCurves().add(text);

						// System.err.print( " " + text.getText() );
						break;
					}
				}
			}
			// System.err.println( "" );

			annotationList.add(annotation);
		}
		tempList = null;
		timesList = null;

		int maxCount = -1;
		int maxCountIndex = -1;
		for (int i = 0; i < annotationList.size(); i++) {
			if (annotationList.elementAt(i).getCurves().size() > maxCount) {
				maxCount = annotationList.elementAt(i).getCurves().size();
				maxCountIndex = i;
			}
		}
		annotationNames = new Vector<String>();
		for (int i = 0; i < annotationList.elementAt(maxCountIndex).getCurves().size(); i++) {
			VOIText text = (VOIText) annotationList.elementAt(maxCountIndex).getCurves().elementAt(i);
			annotationNames.add(new String(text.getText()));
		}

		// System.err.println( min );
		// System.err.println( max );

		extents[0] = (int) Math.max(30, (max.X - min.X) + 10);
		extents[1] = (int) Math.max(30, (max.Y - min.Y) + 10);
		extents[2] = (int) Math.max(30, (max.Z - min.Z) + 10);

		ModelImage animationImage = new ModelImage(ModelStorageBase.BOOLEAN, extents, "animationImage");
		String outputDirName = baseFileDir[0] + File.separator + "animation" + File.separator;
		final File outputFileDir = new File(outputDirName);

		if (outputFileDir.exists() && outputFileDir.isDirectory()) {
			String[] list = outputFileDir.list();
			for (int i = 0; i < list.length; i++) {
				File lrFile = new File(outputFileDir + list[i]);
				lrFile.delete();
			}
		} else if (outputFileDir.exists() && !outputFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			outputFileDir.mkdir();
		}

		progress.setTitle("Creating Animation Viewer...");
		animationImage.setImageDirectory(outputDirName);
		triVolume = new VolumeTriPlanarInterface(animationImage, null);
		triVolume.addConfiguredListener(this);

		progress.dispose();
		progress = null;
	}
	
	/**
	 * User-interface initialization. If the UI is integrated all panels are
	 * displayed in one window. Otherwise the UI is divided into volume display and
	 * separate UI panels.
	 * 
	 * @param integrated
	 */
	private void init() {
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - lattice - 2.0");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		JDialogStandalonePlugin dialogGUI = new JDialogStandalonePlugin();
		GuiBuilder gui = new GuiBuilder(dialogGUI);

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.weightx = 1;
		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		inputsPanel = new JPanel(new GridBagLayout());
		inputsPanel.setBorder(JDialogBase.buildTitledBorder("Input Options"));
		inputsPanel.setForeground(Color.black);

		baseFileLocText = gui.buildFileField("Data directory (marker 1): ", "", false, JFileChooser.DIRECTORIES_ONLY,
				this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon_reg");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridx++;
		reverseSequence = gui.buildCheckBox("reverse orderr", false);
		inputsPanel.add(reverseSequence.getParent(), gbc);
		gbc.gridx = 0;
		gbc.gridy++;

		loadLegacyAnnotationsCheck = gui.buildCheckBox("convert legacy annotationVOIs.lbl", false);
		inputsPanel.add(loadLegacyAnnotationsCheck.getParent(), gbc);
		gbc.gridy++;

		loadLegacyLatticeCheck = gui.buildCheckBox("convert legacy lattice.xml", false);
		inputsPanel.add(loadLegacyLatticeCheck.getParent(), gbc);
		gbc.gridy++;

		JPanel startPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		startPanel.add(startButton);
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		startPanel.add(closeButton);
		startPanel.add(new JPanel());

		optionsPanel = makeOptionsPanel(gui);

		ButtonGroup group = new ButtonGroup();
		algorithmsPanel = makeAlgorithmsPanel(gui, group);
		editPanel = makeEditPanel(gui, group);

		choicePanel = new JPanel(new GridLayout(1, 2));
		choicePanel.add(algorithmsPanel);
		choicePanel.add(editPanel);

		JPanel panel1 = new JPanel(new BorderLayout());
		panel1.add(inputsPanel, BorderLayout.NORTH);
		panel1.add(optionsPanel, BorderLayout.SOUTH);

		JPanel panel2 = new JPanel(new BorderLayout());
		panel2.add(choicePanel, BorderLayout.NORTH);
		panel2.add(startPanel, BorderLayout.SOUTH);

		dialogGUI.getContentPane().add(panel1, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(panel2, BorderLayout.SOUTH);

		lutPanel = new JPanel(new GridLayout(2,1));
		lutTab = new JTabbedPane();
		lutTab.addChangeListener(this);
		lutPanel.add(lutTab, BorderLayout.CENTER);
		opacityPanel = new JPanel( new BorderLayout() );
		opacityTab = new JTabbedPane();
		opacityTab.addChangeListener(this);
		opacityPanel.add(opacityTab, BorderLayout.CENTER);
		
		clipPanel = new JPanel( new BorderLayout() );
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab("LUT", null, lutPanel);
		tabbedPane.addTab("Opacity", null, opacityPanel);
		tabbedPane.addTab("Clip", null, clipPanel);
		tabbedPane.setVisible(false);
		tabbedPane.addChangeListener(this);

		displayControls = new JPanel(new BorderLayout());
		displayControls.add(dialogGUI.getContentPane(), BorderLayout.NORTH);
		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.add(displayControls, BorderLayout.NORTH);
		
		imageChannels = new JPanel();
		imageChannels.add(new JLabel("Select image channel:") );
		imageChannels.setVisible(false);
		leftPanel.add(imageChannels, BorderLayout.CENTER);
		leftPanel.add(tabbedPane, BorderLayout.SOUTH);

		JScrollPane scroller = new JScrollPane(leftPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

		volumePanel = initGPUPanel(EditNONE);
		volumePanel.setVisible(false);

		integratedPanel = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, scroller, volumePanel);
		integratedPanel.setOneTouchExpandable(true);
		integratedPanel.setDividerSize(6);
		integratedPanel.setContinuousLayout(true);
		integratedPanel.setResizeWeight(0);
		integratedPanel.setDividerLocation(0.25);

		// JPanel integratedPanel = new JPanel( new BorderLayout() );
		// integratedPanel.add( scroller, BorderLayout.WEST );
		// integratedPanel.add( volumePanel, BorderLayout.EAST );
		getContentPane().add(integratedPanel, BorderLayout.CENTER);

		setLocation(0, 0);
		pack();
		setResizable(true);

		segmentSeamCells.setSelected(true);
	}

	/**
	 * Sets up the GPU volume display panel, with the 'back' and 'next' buttons for
	 * going through the images and editing the seam cells or lattices.
	 * 
	 * @param editMode when the edit mode is for editing lattices up to 5 lattice
	 *                 options are shown to the user.
	 * @return
	 */
	private Container initGPUPanel(int editMode) {
		MipavInitGPU.InitGPU();

		JDialogStandalonePlugin dialogGUI = new JDialogStandalonePlugin();
		gui = new GuiBuilder(dialogGUI);

		backNextPanel = new JPanel();
		backButton = gui.buildButton("back");
		backButton.addActionListener(this);
		backButton.setActionCommand("back");
		backButton.setVisible(true);
		backButton.setEnabled(false);
		backNextPanel.add(backButton);

		nextButton = gui.buildButton("next");
		nextButton.addActionListener(this);
		nextButton.setActionCommand("next");
		nextButton.setVisible(true);
		nextButton.setEnabled(true);
		backNextPanel.add(nextButton);

		doneButton = gui.buildButton("done");
		doneButton.addActionListener(this);
		doneButton.setActionCommand("done");
		doneButton.setVisible(true);
		doneButton.setEnabled(true);
		backNextPanel.add(doneButton);
		
		JButton demo = gui.buildButton("demo");
		demo.addActionListener(this);
		demo.setActionCommand("demo");
		demo.setVisible(true);
		demo.setEnabled(true);
//		backNextPanel.add(demo);

		latticeSelectionPanel = new JPanel();
		
		newLatticeButton = gui.buildButton("new lattice");
		newLatticeButton.addActionListener(this);
		newLatticeButton.setActionCommand("newLattice");
		newLatticeButton.setVisible(true);
		newLatticeButton.setEnabled(true);
		latticeSelectionPanel.add(newLatticeButton);

		flipLatticeButton = gui.buildButton("flip lattice");
		flipLatticeButton.addActionListener(this);
		flipLatticeButton.setActionCommand("flipLattice");
		flipLatticeButton.setVisible(true);
		flipLatticeButton.setEnabled(true);
		latticeSelectionPanel.add(flipLatticeButton);

		displayModel = gui.buildCheckBox("Show Model", false);
		displayModel.addActionListener(this);
		displayModel.setActionCommand("displayModel");
		displayModel.setVisible(false);
		displayModel.setEnabled(true);
		latticeSelectionPanel.add(displayModel);
		
		displaySurface = gui.buildCheckBox("Show Surface", false);
		displaySurface.addActionListener(this);
		displaySurface.setActionCommand("displaySurface");
		displaySurface.setVisible(false);
		displaySurface.setEnabled(true);
		
		editCrossSections = gui.buildCheckBox("Edit Sections", false);
		editCrossSections.addActionListener(this);
		editCrossSections.setActionCommand("editCrossSections");
		editCrossSections.setVisible(false);
		editCrossSections.setEnabled(true);
		latticeSelectionPanel.add(editCrossSections);

		predict = gui.buildButton("predict");
		predict.addActionListener(this);
		predict.setActionCommand("predict");
		predict.setVisible(false);
		predict.setEnabled(true);
		latticeSelectionPanel.add(predict);

		previewUntwisting = gui.buildButton("preview");
		previewUntwisting.addActionListener(this);
		previewUntwisting.setActionCommand("preview");
		previewUntwisting.setVisible(false);
		previewUntwisting.setEnabled(true);
		latticeSelectionPanel.add(previewUntwisting);

		backNextPanel.add(latticeSelectionPanel);
		latticeSelectionPanel.setVisible(false);

		gpuPanel = new JPanel(new BorderLayout());
		dialogGUI.getContentPane().add(gpuPanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(backNextPanel, BorderLayout.SOUTH);

		return dialogGUI.getContentPane();
	}

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	private void initDisplayAnnotationsPanel(VolumeTriPlanarRender renderer, VOILatticeManagerInterface manager,
			IntegratedWormData image) {
		if (image.annotationPanelUI == null) {
			image.annotationPanelUI = new JPanelAnnotations(manager, renderer, image.volumeImage);
		}
		image.annotationPanelUI.initDisplayAnnotationsPanel(manager, image.volumeImage, true, (dualGPU != null));

		if (dualGPU != null) {
			if (leftImage.annotationPanelUI != null && rightImage.annotationPanelUI != null) {
				if (annotationPanels == null) {

					annotationPanels = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
							leftImage.annotationPanelUI.getAnnotationsPanel(),
							rightImage.annotationPanelUI.getAnnotationsPanel());
					annotationPanels.setOneTouchExpandable(true);
					annotationPanels.setDividerSize(6);
					annotationPanels.setContinuousLayout(true);
					annotationPanels.setResizeWeight(0.5);
					annotationPanels.setDividerLocation(0.5);

					tabbedPane.addTab("Annotation", null, annotationPanels);
					pack();
				} else if (nextBackFlag) {
					annotationPanels.removeAll();
					annotationPanels.add(leftImage.annotationPanelUI.getAnnotationsPanel());
					annotationPanels.add(rightImage.annotationPanelUI.getAnnotationsPanel());
				}
			}
		} else if (annotationPanelSingle == null) {
			annotationPanelSingle = new JPanel(new BorderLayout());
			annotationPanelSingle.add(leftImage.annotationPanelUI.getAnnotationsPanel(), BorderLayout.NORTH);
			tabbedPane.addTab("Annotation", null, annotationPanelSingle);
			pack();
		}
		if (dualGPU != null) {
			if (leftImage.annotationPanelUI != null && rightImage.annotationPanelUI != null) {
				leftImage.annotationPanelUI.setSharedAnnotationPanel(rightImage.annotationPanelUI);
				rightImage.annotationPanelUI.setSharedAnnotationPanel(leftImage.annotationPanelUI);
			}
		}
	}

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	private void initDisplayCurvesPanel(VolumeTriPlanarRender renderer, VOILatticeManagerInterface manager,
			IntegratedWormData image) {
		if (image.curvesPanelUI == null) {
			image.curvesPanelUI = new JPanelCurves(image.voiManager, renderer, image.volumeImage);
		}
		image.curvesPanelUI.initDisplayCurvesPanel(image.voiManager, image.volumeImage, true);

		if (dualGPU != null) {
			if (leftImage.curvesPanelUI != null && rightImage.curvesPanelUI != null) {
				if (curvePanel == null) {

					curvePanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT, leftImage.curvesPanelUI.getCurvesPanel(),
							rightImage.curvesPanelUI.getCurvesPanel());
					curvePanel.setOneTouchExpandable(true);
					curvePanel.setDividerSize(6);
					curvePanel.setContinuousLayout(true);
					curvePanel.setResizeWeight(0.5);
					curvePanel.setDividerLocation(0.5);

					tabbedPane.addTab("Curves", null, curvePanel);
					pack();
				} else if (nextBackFlag) {
					curvePanel.removeAll();
					curvePanel.add(leftImage.curvesPanelUI.getCurvesPanel());
					curvePanel.add(rightImage.curvesPanelUI.getCurvesPanel());
				}
			}
		} else {
			if (curvePanelSingle == null) {
				curvePanelSingle = new JPanel(new BorderLayout());
				curvePanelSingle.add(leftImage.curvesPanelUI.getCurvesPanel(), BorderLayout.NORTH);
				tabbedPane.addTab("Curves", null, curvePanelSingle);
				pack();
			}
		}
	}

	private void initDisplayLatticePanel(VolumeTriPlanarRender renderer, VOILatticeManagerInterface manager,
			IntegratedWormData image) {
		// System.err.println("initDisplayLatticePanel");
		if (image.latticeTable == null) {
			image.latticeTable = new JPanelLattice(image.voiManager, image.volumeImage.GetImage());
		}
		image.latticeTable.initDisplayAnnotationsPanel(image.voiManager, image.volumeImage.GetImage());

		if (dualGPU != null) {
			if (leftImage.latticeTable != null && rightImage.latticeTable != null) {
				if (latticePanel == null) {

					latticePanel = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
							leftImage.latticeTable.getAnnotationsPanel(),
							rightImage.latticeTable.getAnnotationsPanel());
					latticePanel.setOneTouchExpandable(true);
					latticePanel.setDividerSize(6);
					latticePanel.setContinuousLayout(true);
					latticePanel.setResizeWeight(0.5);
					latticePanel.setDividerLocation(0.5);

					tabbedPane.addTab("Lattice", null, latticePanel);
					pack();
				} else if (nextBackFlag) {
					latticePanel.removeAll();
					latticePanel.add(leftImage.latticeTable.getAnnotationsPanel());
					latticePanel.add(rightImage.latticeTable.getAnnotationsPanel());
				}
			}
		} else if (latticePanelSingle == null) {
			latticePanelSingle = new JPanel(new BorderLayout());
			latticePanelSingle.add(leftImage.latticeTable.getAnnotationsPanel(), BorderLayout.NORTH);
			tabbedPane.addTab("Lattice", null, latticePanelSingle);
			pack();
		}
	}

	// private JPanelAnnotations checkSeamPanel = null;
	// private void initDisplaySeamPanel() {
	//
	// System.err.println("initDisplaySeamPanel");
	// if ( checkSeamPanel == null )
	// {
	// checkSeamPanel = new JPanelAnnotations(voiManager, volumeImage.GetImage());
	// checkSeamPanel.initDisplayAnnotationsPanel(voiManager,
	// volumeImage.GetImage(), true);
	// tabbedPane.addTab("Seam Cells", null, checkSeamPanel.getAnnotationsPanel());
	// pack();
	// }
	// checkSeamPanel.initDisplayAnnotationsPanel(voiManager,
	// volumeImage.GetImage(), true);
	// }

	/**
	 * Builds the algorithms panel for automatic seam-cell detection, automatic
	 * lattice building, straightening, etc. Sets up the buttons and return the
	 * panel.
	 * 
	 * @param gui
	 * @param group
	 * @return the user-interface panel.
	 */
	private JPanel makeAlgorithmsPanel(GuiBuilder gui, ButtonGroup group) {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
		// gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Batch Algorithms"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		segmentSeamCells = gui.buildRadioButton("1). segment seam cells", true);
		segmentSeamCells.addActionListener(this);
		// panel.add(segmentSeamCells.getParent(), gbc);
		// gbc.gridy++;

		gbc.gridx = 0;
		batchFlipLattice = gui.buildRadioButton("Flip lattices", false);
		batchFlipLattice.addActionListener(this);
		panel.add(batchFlipLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		latticeStraighten = gui.buildRadioButton("straighten", false);
		latticeStraighten.addActionListener(this);
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		calcMaxProjection = gui.buildRadioButton("generate maximum intensity projection animation", false);
		calcMaxProjection.addActionListener(this);
		calcMaxProjection.setEnabled(false);
		panel.add(calcMaxProjection.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		resliceRotate = gui.buildRadioButton("Reslice and rotate", false);
		resliceRotate.addActionListener(this);
		resliceRotate.setEnabled(false);
		panel.add(resliceRotate.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		generateModelMesh = gui.buildRadioButton("Generate triangle mesh from lattice", false);
		generateModelMesh.addActionListener(this);
		generateModelMesh.setEnabled(false);
		panel.add(generateModelMesh.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		batchProgress = new JProgressBar(0, 100);
		panel.add(batchProgress, gbc);

		group.add(segmentSeamCells);
		group.add(batchFlipLattice);
		group.add(latticeStraighten);
		group.add(calcMaxProjection);
		group.add(resliceRotate);
		group.add(generateModelMesh);

		return panel;
	}

	/**
	 * Generates the panel for editing seam cells, lattices, annotations, inspecting
	 * the straightened image results, etc.
	 * 
	 * @param gui
	 * @param group
	 * @return user-interface panel.
	 */
	private JPanel makeEditPanel(GuiBuilder gui, ButtonGroup group) {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
		// gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Build / Edit"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		editSeamCells = gui.buildRadioButton("2). edit seam cells", false);
		editSeamCells.setEnabled(true);
		editSeamCells.addActionListener(this);
		editSeamCells.setActionCommand("editSeamCells");
		// panel.add(editSeamCells.getParent(), gbc);
		// gbc.gridy++;

		gbc.gridx = 0;
		editLattice = gui.buildRadioButton("edit lattice", false);
		editLattice.setEnabled(true);
		editLattice.addActionListener(this);
		editLattice.setActionCommand("editLattice");
		panel.add(editLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		checkSeamCells = gui.buildRadioButton("4b). check seam cells", false);
		checkSeamCells.setEnabled(true);
		checkSeamCells.addActionListener(this);
		checkSeamCells.setActionCommand("checkSeamCells");
		// panel.add(checkSeamCells.getParent(), gbc);
		// gbc.gridy++;

		gbc.gridx = 0;
		integratedEdit = gui.buildRadioButton("Integrated Editing", false);
		integratedEdit.addActionListener(this);
		integratedEdit.setActionCommand("integratedEdit");
		panel.add(integratedEdit.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		reviewResults = gui.buildRadioButton("review straightened results", false);
		reviewResults.setEnabled(true);
		reviewResults.addActionListener(this);
		reviewResults.setActionCommand("reviewResults");
		panel.add(reviewResults.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		createAnimation = gui.buildRadioButton("create annotation animation", false);
		createAnimation.setEnabled(true);
		createAnimation.addActionListener(this);
		createAnimation.setActionCommand("createAnimation");
		panel.add(createAnimation.getParent(), gbc);
		gbc.gridy++;

		group.add(editSeamCells);
		group.add(editLattice);
		group.add(checkSeamCells);
		group.add(createAnimation);
		group.add(integratedEdit);
		group.add(reviewResults);

		return panel;
	}

	private JPanel makeOptionsPanel(GuiBuilder gui) {
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
		// gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Options"));
		panel.setForeground(Color.black);

		seamCellMinRadiusText = gui.buildField("Seam cell min radius (voxels): ", "           8");
		panel.add(seamCellMinRadiusText.getParent(), gbc);
		gbc.gridy++;

		seamCellMaxRadiusText = gui.buildField("Seam cell max radius (voxels): ", "           25");
		panel.add(seamCellMaxRadiusText.getParent(), gbc);
		gbc.gridy++;

		segmentationPaddingText = gui.buildField("Segmentation padding (voxels): ", "          5");
		segmentationPaddingText.addActionListener(this);
		segmentationPaddingText.setActionCommand("Padding");
		panel.add(segmentationPaddingText.getParent(), gbc);
		gbc.gridy++;

		thresholdImageCheck = gui.buildCheckBox("Threshold image (volume viewing)", false);
		panel.add(thresholdImageCheck.getParent(), gbc);
		gbc.gridx++;

		thresholdValue = gui.buildField("value: ", "75");
		panel.add(thresholdValue.getParent(), gbc);
		gbc.gridx++;

		gbc.gridx = 0;
		gbc.gridy++;

		modelStraightenCheck = gui.buildCheckBox("Segment straightened image with lattice", false);
		panel.add(modelStraightenCheck.getParent(), gbc);
		gbc.gridx++;

		gbc.gridx = 0;
		gbc.gridy++;

		resliceImageCheck = gui.buildCheckBox("Reslice straightened", true);
		panel.add(resliceImageCheck.getParent(), gbc);
		gbc.gridx++;

		resliceX = gui.buildField("x:", String.valueOf(resliceXValue));
		panel.add(resliceX.getParent(), gbc);
		gbc.gridx++;
		gbc.gridx++;

		resliceY = gui.buildField("  y:", String.valueOf(resliceYValue));
		panel.add(resliceY.getParent(), gbc);
		gbc.gridx++;
		gbc.gridx++;

		resliceZ = gui.buildField("  z:", String.valueOf(resliceZValue));
		panel.add(resliceZ.getParent(), gbc);
		gbc.gridx++;

		gbc.gridx = 0;
		gbc.gridy++;

		return panel;
	}

	/**
	 * Saves seam cells, lattice, or annotations based on the current edit mode.
	 */
	private void save() {
		if (editMode == EditSeamCells) {
			saveSeamCells();
		} else if (editMode == EditLattice) {
			saveLattice();
		} else if (editMode == CheckSeam) {
			saveSeamCells();
		} else if (editMode == IntegratedEditing) {
			saveIntegrated();
		}
	}

	private void saveAll() {
		if (editMode == ReviewResults)
			return;
		if (imageStack != null) {
			int endIndex = dualGPU == null ? imageIndex + 1 : imageIndex + 2;
			for (int i = imageIndex; i < endIndex; i++) {
				activeImage = imageStack[i];
				System.err.println("saveAll " + i + " " + activeImage.wormImage.getImageName());
				saveIntegrated();
			}
		}
	}

	/**
	 * Saves the lattice to the default edited file for the current image.
	 */
	private void saveLattice() {
		if (activeImage == null || activeImage.wormImage == null || activeImage.voiManager == null) {
			return;
		}
		activeImage.voiManager.saveLattice(resultsDir(latticeFileDir, activeImage.wormImage) + File.separator,
				PlugInAlgorithmWormUntwistingJanelia.editLatticeOutput);

	}

	private void saveSplineCurves() {
		if (activeImage == null || activeImage.wormImage == null || activeImage.voiManager == null) {
			return;
		}
		String imageName = activeImage.wormImage.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		activeImage.voiManager.saveNeuriteCurves();

	}

	private void openNeuriteCurves(IntegratedWormData data) {
		if (editMode == IntegratedEditing) {
			if (data.voiManager != null) {
				data.voiManager.openNeuriteCurves(resultsDir(latticeFileDir, data.wormImage));
			}
		}
		if (editMode == ReviewResults) {
			LatticeModel.openStraightNeuriteCurves(data.wormImage, resultsDir(latticeFileDir, data.wormImage));
		}
	}

	private void loadPredicted(IntegratedWormData data) {
		// load and save the straightened predicted values:
		VOI markerAnnotations = LatticeModel.readAnnotationsCSV(resultsDir(latticeFileDir, data.wormImage) + File.separator
				+ "prediction" + File.separator + "predicted_straightened_annotations.csv");
		if (markerAnnotations != null) {
			LatticeModel.saveAnnotationsAsCSV( resultsDir(latticeFileDir, data.wormImage) + File.separator + "straightened_annotations",
					"straightened_annotations.csv", markerAnnotations);
		}
		// load, save and display the predicted annotations:
		if (data.annotations != null) {
			data.annotations.clear();
			data.annotations = null;
		}
		data.annotations = new VOIVector();
		markerAnnotations = LatticeModel.readAnnotationsCSV(resultsDir(latticeFileDir, data.wormImage) + File.separator
				+ "prediction" + File.separator + "predicted_annotations.csv");
		if (markerAnnotations != null) {
			data.voiManager.deleteAnnotations();
			System.err.println(markerAnnotations + "  " + markerAnnotations.getCurves().size());
			data.annotations.add(markerAnnotations);
			data.voiManager.addAnnotations(data.annotations);
			WormData.saveIntegratedMarkerAnnotations(resultsDir(latticeFileDir, data.wormImage), data.voiManager.getAnnotations());
			data.annotations = null;

			VOI annotations = data.voiManager.getAnnotations();
			if (annotations != null) {
				for (int i = 0; i < annotations.getCurves().size(); i++) {
					final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
					text.createVolumeVOI(data.volumeImage, activeRenderer.getTranslate());
				}
			}
		}
	}

	private void openAnnotations(IntegratedWormData data) {
//		private void openAnnotations(IntegratedWormData data, String dir1, String dir2, int editMode) {
		System.err.println("openAnnotations " + data.wormImage.getImageFileName());
		if (data.annotations != null) {
			data.annotations.clear();
			data.annotations = null;
		}
		data.annotations = new VOIVector();

		if (editMode == IntegratedEditing) {
			if (WormData.integratedExists(resultsDir(latticeFileDir, data.wormImage))) {
				VOI markers = WormData.getIntegratedMarkerAnnotations(resultsDir(latticeFileDir, data.wormImage));
				if (markers != null) {
					data.annotations.add(markers);
				}
			} 
			else {
//				if (loadLegacyAnnotationsCheck.isSelected()) {
//					data.wormImage.setResolutions(originalResolutions);
//				}
//
//				// read the original imageA/imageB markers - saved to the new integrated dir
//				VOI markers = data.wormData.getMarkerAnnotations(dir1);
//				if (markers != null) {
//					data.annotations.add(markers);
//				}
//				markers = data.wormData.getMarkerAnnotations(dir2);
//				if (markers != null) {
//					data.annotations.add(markers);
//				}
//
//				if (loadLegacyAnnotationsCheck.isSelected()) {
//					data.wormImage.setResolutions(new float[] { 1f, 1f, 1 });
//				}
//
//				data.wormData.saveIntegratedMarkerAnnotations(data.voiManager.getAnnotations());
			}
		} else if (editMode == ReviewResults) {
			WormData.openStraightAnnotations(resultsDir(latticeFileDir, data.wormImage), data.wormImage);
		}
		if (data.annotations.size() > 0)
			System.err.println("openAnnotations " + data.wormImage.getImageName() + "   "
					+ data.annotations.elementAt(0).getCurves().size());
	}
	
	/**
	 * Saves the seam cells to the default edited file for the current image.
	 */
	private void saveSeamCells() {
		if (activeImage.wormImage == null) {
			return;
		}
		if (imageIndex >= includeRange.size()) {
			return;
		}
		WormData.saveSeamAnnotations(resultsDir(latticeFileDir, activeImage.wormImage), activeImage.voiManager.getAnnotations(),
				(editMode != CheckSeam) && (editMode != IntegratedEditing), true);
	}

	/**
	 * Sets the include range list of file IDs when the user presses 'start'.
	 * 
	 * @return true if there are files in the list to process.
	 */
	private boolean setVariables() {
		try {
			paddingFactor = Integer.valueOf(segmentationPaddingText.getText().trim());
		} catch (NumberFormatException e) {
			paddingFactor = 0;
		}
		try {
			minRadius = Integer.valueOf(seamCellMinRadiusText.getText().trim());
		} catch (NumberFormatException e) {
			minRadius = 8;
		}
		try {
			maxRadius = Integer.valueOf(seamCellMaxRadiusText.getText().trim());
		} catch (NumberFormatException e) {
			maxRadius = 25;
		}
		try {
			resliceXValue = Integer.valueOf(resliceX.getText().trim());
		} catch (NumberFormatException e) {
			resliceXValue = 250;
		}
		try {
			resliceYValue = Integer.valueOf(resliceY.getText().trim());
		} catch (NumberFormatException e) {
			resliceYValue = 250;
		}
		try {
			resliceZValue = Integer.valueOf(resliceZ.getText().trim());
		} catch (NumberFormatException e) {
			resliceZValue = 1500;
		}
		try {
			threshold = -1;
			if (thresholdImageCheck.isSelected()) {
				threshold = Integer.valueOf(thresholdValue.getText().trim());
			}
		} catch (NumberFormatException e) {
			threshold = 75;
		}

		baseFileName = baseFileNameText.getText();
		
		includeRange = new Vector<Integer>();
		String rangeFusion = rangeFusionText.getText();
		if (rangeFusion != null) {
			String[] ranges = rangeFusion.split("[,;]");
			for (int i = 0; i < ranges.length; i++) {
				String[] subset = ranges[i].split("-");
				int lowerBound = -1, bound = -1;
				for (int j = 0; j < subset.length; j++) {
					try {
						bound = Integer.valueOf(subset[j].trim());
						if (lowerBound == -1) {
							lowerBound = bound;
							includeRange.add(lowerBound);
						}
					} catch (NumberFormatException e) {
						Preferences.debug("Invalid range specified: " + bound, Preferences.DEBUG_ALGORITHM);
					}
				}

				for (int k = lowerBound + 1; k <= bound; k++) {
					includeRange.add(k);
				}
			}
		}

		if (includeRange.size() == 0) {
			includeRange = null;
		}
		if ((includeRange != null) && reverseSequence.isSelected()) {
			// reverse the order of images:
			int[] temp = new int[includeRange.size()];
			int count = 0;
			for (int i = includeRange.size() - 1; i >= 0; i--) {
				temp[count++] = includeRange.remove(i);
			}
			for (int i = 0; i < temp.length; i++) {
				includeRange.add(temp[i]);
			}
		}
		imageIndex = 0;

		return (includeRange != null);
	}

	private boolean imageChannelsInit = false;
	private void initImageChannels(IntegratedWormData integratedData) {
		if ( imageChannelsInit ) return;
		imageChannelsInit = true;
		int numImages = integratedData.hyperstack.length;
		String[] subDir = new String[numImages];
		for ( int i = 0; i < numImages; i++ ) {
			int index = baseFileDir[i].lastIndexOf(File.separator) + 1;
			int len = baseFileDir[i].length();
			subDir[i] = baseFileDir[i].substring(index,len);
			JCheckBox box = new JCheckBox(subDir[i], true);
			box.setActionCommand(subDir[i]);
			box.addActionListener(this);
			imageChannels.add( box );
		}
	}
	
	private int imageChannel(String cmd) {

		int numImages = activeImage.hyperstack.length;
		for ( int i = 0; i < numImages; i++ ) {
			int index = baseFileDir[i].lastIndexOf(File.separator) + 1;
			int len = baseFileDir[i].length();
			String subDir = baseFileDir[i].substring(index,len);
			if ( subDir.equals(cmd) ) {
				return i;
			}
		}
		return -1;
	}
	
	/**
	 * Creates or updates the histogram / LUT panel and opacity panels when a new
	 * image is loaded.
	 */
	private void initHistoLUTPanel(IntegratedWormData integratedData) {
//		GridBagConstraints gbc = new GridBagConstraints();
//		gbc.gridx = 0;
//		gbc.gridy = 0;
//		gbc.weightx = 1;
//		gbc.weighty = 0;
//		gbc.anchor = GridBagConstraints.WEST;
//		integratedData.colorChannelPanel = new JPanel(new GridBagLayout());
//		if (integratedData.wormImage.isColorImage()) {
//			ButtonGroup group = new ButtonGroup();
//			integratedData.displayChannel1 = gui.buildRadioButton("Channel 1", false);
//			integratedData.displayChannel1.addActionListener(this);
//			integratedData.displayChannel1.setActionCommand("displayChannel1");
//			integratedData.displayChannel1.setVisible(true);
//			integratedData.displayChannel1.setEnabled(true);
//			group.add(integratedData.displayChannel1);
//			integratedData.colorChannelPanel.add(integratedData.displayChannel1, gbc);
//			gbc.gridy++;
//
//			integratedData.displayChannel2 = gui.buildRadioButton("Channel 2", false);
//			integratedData.displayChannel2.addActionListener(this);
//			integratedData.displayChannel2.setActionCommand("displayChannel2");
//			integratedData.displayChannel2.setVisible(true);
//			integratedData.displayChannel2.setEnabled(true);
//			group.add(integratedData.displayChannel2);
//			if ( !baseFileDir[1].isEmpty() ) {
//				integratedData.colorChannelPanel.add(integratedData.displayChannel2, gbc);
//				gbc.gridy++;
//			}
//			
//			integratedData.displayChannel3 = gui.buildRadioButton("Channel 3", false);
//			integratedData.displayChannel3.addActionListener(this);
//			integratedData.displayChannel3.setActionCommand("displayChannel3");
//			integratedData.displayChannel3.setVisible(true);
//			integratedData.displayChannel3.setEnabled(true);
//			group.add(integratedData.displayChannel3);
//			if ( !baseFileDir[2].isEmpty() ) {
//				integratedData.colorChannelPanel.add(integratedData.displayChannel3, gbc);
//				gbc.gridy++;
//			}
//
//			integratedData.displayBothChannels = gui.buildRadioButton("Display All Channels", true);
//			integratedData.displayBothChannels.addActionListener(this);
//			integratedData.displayBothChannels.setActionCommand("displayBothChannels");
//			integratedData.displayBothChannels.setVisible(true);
//			integratedData.displayBothChannels.setEnabled(true);
//			group.add(integratedData.displayBothChannels);
//			integratedData.colorChannelPanel.add(integratedData.displayBothChannels, gbc);
//
//			gbc.gridy++;
//			gbc.weighty = 1;
//			integratedData.colorChannelPanel.add(new JLabel(""), gbc);
//		}

		int numImages = integratedData.hyperstack.length;
		integratedData.volOpacityPanel = new JPanelVolumeOpacity[numImages];
		for ( int i = 0; i < numImages; i++ ) {
			integratedData.volOpacityPanel[i] = new JPanelVolumeOpacity(integratedData.hyperstack[i].GetImage(),
					null, null,	null, true);
			integratedData.volOpacityPanel[i].addPropertyChangeListener(this);
			TransferFunction kTransfer = integratedData.volOpacityPanel[i].getCompA().getOpacityTransferFunction();

			integratedData.hyperstack[i].UpdateImages(kTransfer, 0, null);
		}

		// System.err.println( "initHistoLUTPanel " +
		// integratedData.volumeImage.getLUT() );
		integratedData.lutHistogramPanel = new JFrameHistogram[numImages];
		for ( int i = 0; i < numImages; i++ ) {

			integratedData.lutHistogramPanel[i] = new JFrameHistogram(this, integratedData.hyperstack[i].GetImage(), null,
					integratedData.hyperstack[i].getLUT(), null);
			integratedData.lutHistogramPanel[i].histogramLUT(true, false, false);//!integratedData.volumeImage.GetImage().isColorImage());
		}


//		if (integratedData.volumeImage.GetImage().isColorImage()) {
//			integratedData.displayBothChannels.setSelected(true);
//		}
		for ( int i = 0; i < numImages; i++ ) {
			integratedData.hyperstack[i].GetImage().addImageDisplayListener(this);
		}
	}

	private void updateHistoLUTPanels(IntegratedWormData integratedData) {
//		 System.err.println("updateHistoLUTPanels");
		// integratedData.wormImage.getImageName() + " " +
		// integratedData.lutHistogramPanel.getContainingPanel() );
		int numImages = integratedData.hyperstack.length;
		String[] subDir = new String[numImages];
		if ( !integratedData.colorMapInit ) {
			integratedData.colorMapInit = true;
			for ( int i = 0; i < numImages; i++ ) {
				int index = baseFileDir[i].lastIndexOf(File.separator) + 1;
				int len = baseFileDir[i].length();
				subDir[i] = baseFileDir[i].substring(index,len);
				if ( subDir[i].equals("405") ) {
					// set transfer function:
					ModelLUT lut = integratedData.hyperstack[i].GetLUT();
					lut.makeBlueTransferFunctions();
					TransferFunction tf = lut.getTransferFunction();
					tf.replacePoint(34, 128, 1);
					tf.replacePoint(64, 0, 2);
					lut.makeLUT(256);
				}
				if ( subDir[i].equals("488") ) {
					// set transfer function:
					ModelLUT lut = integratedData.hyperstack[i].GetLUT();
					lut.makeGreenTransferFunctions();
					TransferFunction tf = lut.getTransferFunction();
					tf.replacePoint(34, 128, 1);
					tf.replacePoint(64, 0, 2);
					lut.makeLUT(256);
				}
				if ( subDir[i].equals("561") ) {
					// set transfer function:
					ModelLUT lut = integratedData.hyperstack[i].GetLUT();
					lut.makeRedTransferFunctions();
					TransferFunction tf = lut.getTransferFunction();
					tf.replacePoint(34, 128, 1);
					tf.replacePoint(64, 0, 2);
					lut.makeLUT(256);
				}
				if ( subDir[i].equals("637") ) {
					// set transfer function:
					ModelLUT lut = integratedData.hyperstack[i].GetLUT();
					TransferFunction tf = lut.getTransferFunction();
					tf.replacePoint(34, 128, 1);
					tf.replacePoint(64, 0, 2);
					lut.makeLUT(256);
				}
			}
		}
		
		opacityTab.removeAll();		
		for ( int i = 0; i < numImages; i++ ) {
			int index = baseFileDir[i].lastIndexOf(File.separator) + 1;
			int len = baseFileDir[i].length();
			subDir[i] = baseFileDir[i].substring(index,len);
			
			opacityTab.addTab( subDir[i] + File.separator + integratedData.hyperstack[i].GetImage().getImageName(), 
					null, integratedData.volOpacityPanel[i].getMainPanel() );

			
			final TransferFunction kTransfer = integratedData.volOpacityPanel[i].getCompA().getOpacityTransferFunction();
			updateImages(integratedData.colormap, integratedData.hyperstack[i].GetImage(), kTransfer, i);
			integratedData.volOpacityPanel[i].getCompA().showHistogram();
			integratedData.hyperstack[i].UpdateImages(kTransfer, 0, null);

			if ( integratedData.previewHS != null ) {
				integratedData.previewHS[i].UpdateImages(kTransfer, 0, null);
			}
		}
		if (tabbedPane.getSelectedComponent() == opacityPanel) {
			int which = opacityTab.getSelectedIndex();
			if ( which != -1 ) {
				integratedData.volOpacityPanel[which].getCompA().showHistogram();
			}
		}

		lutTab.removeAll();
		for ( int i = 0; i < numImages; i++ ) {
			lutTab.addTab( subDir[i] + File.separator + integratedData.hyperstack[i].GetImage().getImageName(), 
					null, integratedData.lutHistogramPanel[i].getContainingPanel());
			updateImages(integratedData.colormap, integratedData.hyperstack[i].GetLUT(), i );
			
			integratedData.hyperstack[i].UpdateImages(integratedData.hyperstack[i].getLUT());

			if ( integratedData.previewHS != null ) {
				integratedData.previewHS[i].UpdateImages(integratedData.hyperstack[i].getLUT());
			}
			integratedData.lutHistogramPanel[i].redrawFrames();
		}
		if (tabbedPane.getSelectedComponent() == lutPanel) {
			int which = lutTab.getSelectedIndex();			
			if ( which != -1 ) {
				integratedData.lutHistogramPanel[which].redrawFrames();
			}
		}
		
		
		lutPanel.revalidate();
	}

	private void updateClipPanel(IntegratedWormData integratedData, VolumeTriPlanarRender renderer,
			boolean resetRenderer) {
		if (integratedData.clipGUI == null) {
			integratedData.clipGUI = new JPanelClip_WM(renderer);
		} else if (resetRenderer) {
			integratedData.clipGUI.setRenderer(renderer, false);
		}

		clipPanel.removeAll();
		clipPanel.add(integratedData.clipGUI.getMainPanel(), BorderLayout.WEST);
		integratedData.clipGUI.getMainPanel().repaint();
		clipPanel.validate();

		if (integratedData.clipArb != null) {
			renderer.setArbitratyClip(integratedData.clipArb, integratedData.clipArbOn);
		}
	}

	private JPanelLights_WM lightsPanel = null;

	// private JPanelSurface_WM surfaceGUI = null;
	private void updateSurfacePanels() {
		if (lightsPanel == null) {
			lightsPanel = new JPanelLights_WM(activeRenderer);
			lightsPanel.enableLight(0, true);
			lightsPanel.enableLight(1, true);
		}
		activeRenderer.updateLighting(lightsPanel.getAllLights());
		// if ( surfaceGUI == null ) {
		// surfaceGUI = new JPanelSurface_WM(volumeRenderer);
		// tabbedPane.addTab("Surface", null, lightsPanel);
		// }
	}

	/*
	 * private VOIVector autoLattice() { VOIVector latticeContainer = new
	 * VOIVector(); short id = (short) wormImage.getVOIs().getUniqueID(); VOI
	 * lattice = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());
	 * VOIContour left = new VOIContour(false); VOIContour right = new
	 * VOIContour(false); lattice.getCurves().add(left);
	 * lattice.getCurves().add(right); final int dimX =
	 * wormImage.getExtents().length > 0 ? wormImage.getExtents()[0] : 1; final int
	 * dimY = wormImage.getExtents().length > 1 ? wormImage.getExtents()[1] : 1;
	 * final int dimZ = wormImage.getExtents().length > 2 ?
	 * wormImage.getExtents()[2] : 1; int xLeft = Math.max( 10, dimX/2 - 20); int
	 * xRight = Math.min( dimX - 10, dimX/2 + 20); int z = dimZ/2; int yStep =
	 * dimY/12; int yOffset = 10; for ( int i = 0; i < 10; i++ ) { left.add( new
	 * Vector3f( xLeft, yOffset + i * yStep, z ) ); right.add( new Vector3f(xRight,
	 * yOffset + i * yStep, z ) ); } latticeContainer.add(lattice); return
	 * latticeContainer; }
	 */

	private VOI annotationBackUp = null;
	private VOI seamCellBackUp = null;
	private boolean seamOpen = false;

	public void stateChanged(ChangeEvent arg0) {
		// System.err.println( tabbedPane.getSelectedIndex() + " " +
		// tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()) );
		if (tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()).equals("Lattice")) {
			activeImage.voiManager.editLattice();
			initDisplayLatticePanel(activeRenderer, activeImage.voiManager, activeImage);
		}
		if (tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()).equals("Curves")) {
			activeImage.voiManager.editAnnotations(false);
			initDisplayCurvesPanel(activeRenderer, activeImage.voiManager, activeImage);
		}
		if (tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()).equals("Annotation")) {
			activeImage.voiManager.editAnnotations(false);
			initDisplayAnnotationsPanel(activeRenderer, activeImage.voiManager, activeImage);
			activeImage.annotationOpen = true;
		}
		if (tabbedPane.getTitleAt(tabbedPane.getSelectedIndex()).equals("Clip")) {
			activeImage.voiManager.editClip();
		}
		activeImage.currentTab = tabbedPane.getSelectedIndex();
	}

	private void saveIntegrated() {
		activeImage.voiManager.setSharedDirectory( resultsDir(latticeFileDir, activeImage.wormImage));

		if (editMode == EditSeamCells || editMode == CheckSeam) {
			saveSeamCells();
			return;
		} else if (editMode == EditLattice) {
			saveLattice();
			return;
		}

		// save lattice:
		saveLattice();
		// save any splines created from annotations - those annotations are saved
		// separately so not untwisted twice:
		saveSplineCurves();

		if ( activeImage.voiManager != null ) {
			// save annotations not in splines:
			WormData.saveIntegratedMarkerAnnotations(resultsDir(latticeFileDir, activeImage.wormImage), activeImage.voiManager.getAnnotations());
		}
	}
	
	private void setDefaultInputList(String dir) {

		File file = new File(dir);
		if ( file.exists() && file.isDirectory() ) {
			final String[] list = file.list();
			String imageList = "";
			for (int i = 0; i < list.length; i++) {
				if ( list[i].endsWith(".tif") ) {
					String temp = list[i].substring(list[i].lastIndexOf("_") + 1, list[i].indexOf(".tif") );
					System.err.println(list[i] + "   " + temp);
					if ( imageList != "" ) imageList += ",";
					imageList += temp;
				}
			}
			if ( imageList != "" ) {
				rangeFusionText.setText(imageList);
			}
		}
	}
	
	private class UntwistDialog extends JDialogBase implements ActionListener {
		private JCheckBox[] volumeChecks;
		private String[] volumeDirs;
		private JTextField latticeOutputDir;
		public UntwistDialog( String baseDir ) {
			File file = new File(baseDir);
			if ( file.exists() && file.isDirectory() ) {
				final String[] list = file.list();
				Vector<String> tempList = new Vector<String>();
				for (int i = 0; i < list.length; i++) {
					File subDir = new File(file.getAbsolutePath() + File.separator + list[i]);
					if ( subDir.exists() && subDir.isDirectory() ) 
					{	
						System.err.println(file.getAbsolutePath() + File.separator + list[i]);
						tempList.add(list[i]);
					}
				}
				if ( tempList.size() > 0 ) {
					init(baseDir, tempList);
					setVisible(true);
				}
			}
		} 

		public void actionPerformed(ActionEvent event) {
			String command = event.getActionCommand();
			Object source = event.getSource();

			System.err.println("UntwistDialog " + command);
			if ( command.equals("Cancel") ) {
				baseFileLocText.setText("");
				latticeOutputDir = null;
				setVisible(false);
			}
			else if ( command.equals("OK") ) {
				int count = 0;
				for ( int i = 0; i < volumeChecks.length; i++ ) {
					if ( volumeChecks[i].isSelected() ) {
						count++;
					}
				}
				if ( count == 0 ) {
					baseFileLocText.setText("");
					latticeOutputDir = null;
				}
				else {
					baseFileDir = new String[count];
					count = 0;
					for ( int i = 0; i < volumeChecks.length; i++ ) {
						if ( volumeChecks[i].isSelected() ) {
							baseFileDir[count++] = volumeDirs[i];
						}
					}
				}
				setVisible(false);
			}
		}
		
		
		private void init( String baseDir, Vector<String> tempList ) {

			setForeground(Color.black);
			setTitle("Untwisting C.elegans - lattice - 2.0");
			try {
				setIconImage(MipavUtil.getIconImage("divinci.gif"));
			} catch (FileNotFoundException e) {
				Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
			}
			JPanel input = new JPanel( new GridLayout(tempList.size() + 1, 1));
			GuiBuilder gui = new GuiBuilder(this);
			volumeChecks = new JCheckBox[tempList.size()];
			volumeDirs = new String[tempList.size()];
			String latticeDir = null;
			for ( int i = 0; i < tempList.size(); i++ ) {
				volumeChecks[i] = gui.buildCheckBox(tempList.elementAt(i), true);
				volumeChecks[i].addActionListener(this);
				input.add(volumeChecks[i].getParent());

				volumeDirs[i] = new String(baseDir + File.separator + tempList.elementAt(i));
				final File subDir = new File(volumeDirs[i]);
				if ( latticeDir == null ) {
					latticeDir =  new String(baseDir + File.separator + tempList.elementAt(i));
				}
				if ( containsLattice(subDir) ) {
					latticeDir =  new String(baseDir + File.separator + tempList.elementAt(i));
				}		
				
			}
			
			latticeOutputDir = gui.buildFileField("Lattice Directory:", latticeDir, false, JFileChooser.DIRECTORIES_ONLY,
					this);
			input.add(latticeOutputDir.getParent());
			
			System.err.println(latticeDir);
			getContentPane().add(input, BorderLayout.CENTER);
			
			JPanel buttonPanel = new JPanel();
			buttonPanel.add(JDialogBase.buildOKButton( "OK", this ));
			buttonPanel.add(JDialogBase.buildCancelButton( "Cancel", this ));

			getContentPane().add(buttonPanel, BorderLayout.SOUTH);

			setModal(true);
			if ( bar != null ) bar.setVisible(false);			
			pack();
		}
		
		private boolean containsLattice(File dir ) {

			if ( dir.exists() && dir.isDirectory() ) {
				final String[] list = dir.list();
				for ( int i = 0; i < list.length; i++ ) {
					if ( list[i].equals("lattice_final") ) {
						return true;
					}
					else {
						File file = new File(dir.getAbsolutePath() + File.separator + list[i] );
						if ( file.exists() && file.isDirectory() ) {
							if ( containsLattice(file) ) return true;
						}
					}
				}
			}
			return false;
		}
	}
	
	public void setDemoValues() {
		//baseFileLocText.setText("\\\\nearline4.hhmi.org\\shroff\\shrofflab\\efn-1\\Tracking\\Pos0\\For_Tracking");
		baseFileLocText.setText("D:\\shroff\\For_Tracking");
		editLattice.setSelected(true);
		//actionPerformed(new ActionEvent(this, 0, "BrowseConclude"));
		
		String[] volumeDirs;
		
		String baseDir = baseFileLocText.getText();
		File file = new File(baseDir);
		
		final String[] list = file.list();
		Vector<String> tempList = new Vector<String>();
		for (int i = 0; i < list.length; i++) {
			File subDir = new File(file.getAbsolutePath() + File.separator + list[i]);
			if ( subDir.exists() && subDir.isDirectory() ) 
			{	
				System.err.println(file.getAbsolutePath() + File.separator + list[i]);
				tempList.add(list[i]);
			}
		}
		
		volumeDirs = new String[tempList.size()];
		
		


		int count = 0;
		for ( int i = 0; i < volumeDirs.length; i++ ) {
				count++;
			volumeDirs[i] = new String(baseDir + File.separator + tempList.elementAt(i));
		}
		if ( count == 0 ) {
			baseFileLocText.setText("");
			//latticeOutputDir = null;
		}
		else {
			baseFileDir = new String[count];
			count = 0;
			for ( int i = 0; i < volumeDirs.length; i++ ) {
				baseFileDir[count++] = volumeDirs[i];
			}
		}

		//baseFileLocText.setText("\\\\nearline4.hhmi.org\\shroff\\shrofflab\\efn-1\\Tracking\\Pos0\\For_Tracking\\RegB");

		latticeFileDir = new String("D:\\shroff\\For_Tracking\\RegB");
		setDefaultInputList(latticeFileDir);
		rangeFusionText.setText("60, 61");
		
		actionPerformed(new ActionEvent(this, 0, "start"));
	}

}

