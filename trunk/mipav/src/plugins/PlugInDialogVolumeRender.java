
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
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBConcat;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
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
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JPanelClip_WM;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.JPanelAnnotations;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

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
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * Implements the user-interface for the semi-automatic straightening of the worm.
 * Provides batch-process algorithms for segmenting the seam cells and building lattices.
 * Provides the framework for enabling the user to step through the selected image volumes
 * and view/edit results from the automatic processes.
 * Provides framework for animating the annotations after untwisting.
 */
public class PlugInDialogVolumeRender extends JFrame implements ActionListener, AlgorithmInterface, PropertyChangeListener, ViewImageUpdateInterface, WindowListener {

	private static final long serialVersionUID = -9056581285643263551L;
	
	public static final int EditNONE = 0;
	public static final int EditSeamCells = 1;
	public static final int EditLattice = 2;
	public static final int CheckSeam = 3;
	public static final int EditAnnotations1 = 4;
	public static final int EditAnnotations2 = 5;
	public static final int ReviewResults = 6;
	
	private JPanel algorithmsPanel;
	private VOIVector annotationList;
	private Vector<String> annotationNames;
	private VOIVector annotations;
	private JButton backButton;
	private String baseFileDir;
	private String baseFileDir2;
	private JTextField  baseFileLocText;
	private JTextField  baseFileLocText2;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private JProgressBar batchProgress;
	private JRadioButton buildLattice;
	private JPanel backNextPanel;
	private JRadioButton calcMaxProjection;
	private JRadioButton resliceRotate;
	private JPanel choicePanel;
	private JButton closeButton;
	private JRadioButton createAnimation;
	private JDialogStandalonePlugin dialogGUI;
	private JButton doneButton;
	private JRadioButton editAnnotations1;
	private JRadioButton editAnnotations2;
	private JRadioButton editLattice;
	private JRadioButton checkSeamCells;
	private int editMode = EditNONE;
	private JPanel editPanel;
	private JRadioButton editSeamCells;
	private JPanel gpuPanel;
	private int imageIndex = 0;
	private Vector<Integer> includeRange;
	private JPanel inputsPanel;
	private boolean integratedDisplay = true;
	private JRadioButton[] latticeChoices;
	private JPanel latticeSelectionPanel;
	private JRadioButton latticeStraighten;
	private JFrameHistogram lutHistogramPanel;
	

	private JTextField seamCellMinRadiusText;
	private JTextField seamCellMaxRadiusText;
	private JTextField segmentationPaddingText;
	private JCheckBox thresholdImageCheck;
	private JTextField thresholdValue;
	
	private JCheckBox resliceImageCheck;
	private JTextField resliceX, resliceY, resliceZ;
	private int resliceXValue = 250, resliceYValue = 250, resliceZValue = 1500;
	
	private int paddingFactor;
	private int minRadius;
	private int maxRadius;
	private int threshold = -1;

	private JPanel lutPanel;

	private JButton newLatticeButton; 
	private JButton flipLatticeButton; 
	private JCheckBox displayModel;

	private JButton nextButton;
	
	private JPanel opacityPanel;
	private JPanel clipPanel;
	private JPanelClip_WM clipGUI;
	
	private PlugInDialogVolumeRender parent;
	private VOI finalLattice;
	private VOIVector[] potentialLattices;
	private JTextField rangeFusionText;
	
	private JRadioButton reviewResults;
	
	private JRadioButton segmentSeamCells;
	
	private JButton startButton;

	private JTabbedPane tabbedPane;


	private VolumeTriPlanarInterface triVolume;
	private JPanelAnnotations annotationPanelUI;

	private VOILatticeManagerInterface voiManager;

	private JPanelVolumeOpacity volOpacityPanel;
	private VolumeImage volumeImage;
	private Container volumePanel;
	private VolumeTriPlanarRender volumeRenderer;
	private ModelImage wormImage;
	private WormData wormData;
	
	
	public PlugInDialogVolumeRender()
	{
		this.editMode = EditNONE;
		init(true);
		setVisible(true);
        addWindowListener(this);
	}

	public PlugInDialogVolumeRender( PlugInDialogVolumeRender parent, int mode, Vector<Integer> range, int index, String baseFileDir, String baseFileName )
	{
		this.baseFileDir = baseFileDir;
		this.baseFileName = baseFileName;
		this.parent = parent;
		this.editMode = mode;
		this.includeRange = range;
		this.imageIndex = index;
		init(editMode);
		setVisible(true);
	}
	
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();
		if ( editMode == EditNONE )
		{
			setVariables();
			if (command.equals("start"))
			{
				if ( includeRange == null )
				{
					if ( createAnimation.isSelected() )
					{
						// Launch the animation tool
						this.setVisible(false);
						annotationAnimationFromSpreadSheet();
					}
					else {
						MipavUtil.displayError( "Please specify a range of images." );
						return;
					}
				}
				startButton.setEnabled(false);
				if ( segmentSeamCells.isSelected() )
				{
					try {
					// Batch Automatic Seam Cell Segmentation:
					PlugInAlgorithmWormUntwisting.segmentSeamCells( batchProgress, includeRange, baseFileDir, baseFileNameText.getText(), minRadius, maxRadius );
					} catch ( java.lang.OutOfMemoryError e ) {
						MipavUtil.displayError( "Error: Not enough memory. Unable to finish seam cell segmentation." );
						return;
					}
					segmentSeamCells.setSelected(false);
					editSeamCells.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( buildLattice.isSelected() )
				{
					try {
					// Batch Automatic Lattice-Building
					PlugInAlgorithmWormUntwisting.buildLattice( batchProgress, includeRange, baseFileDir, baseFileNameText.getText());
					} catch ( java.lang.OutOfMemoryError e ) {
						MipavUtil.displayError( "Error: Not enough memory. Unable to finish automatic lattice-building." );
						return;
					}
					editLattice.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( latticeStraighten.isSelected() )
				{
					try {
					// Batch Untwisting:
					PlugInAlgorithmWormUntwisting.latticeStraighten( batchProgress, includeRange, baseFileDir, baseFileDir2, baseFileNameText.getText(), paddingFactor );
					} catch ( java.lang.OutOfMemoryError e ) {
						MipavUtil.displayError( "Error: Not enough memory. Unable to finish straightening." );
						e.printStackTrace();
						return;
					}
					reviewResults.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( calcMaxProjection.isSelected() )
				{
					try {
					// Batch Registration/MP calculation:
					PlugInAlgorithmWormUntwisting.createMaximumProjectionAVI( batchProgress, includeRange,  baseFileDir,  baseFileDir2, baseFileNameText.getText() );
					} catch ( java.lang.OutOfMemoryError e ) {
						MipavUtil.displayError( "Error: Not enough memory. Unable to finish maximum-projection calculation." );
						return;
					}
					startButton.setEnabled(true);
				}
				else if ( resliceRotate.isSelected() )
				{
					try {
						if ( resliceImageCheck.isSelected() )
						{
							PlugInAlgorithmWormUntwisting.reslice( batchProgress, includeRange, baseFileDir, baseFileDir2, baseFileNameText.getText(), 
									resliceXValue, resliceYValue, resliceZValue );
						}					
						} catch ( java.lang.OutOfMemoryError e ) {
						MipavUtil.displayError( "Error: Not enough memory. Unable to finish reslice calculation." );
						return;
					}
					startButton.setEnabled(true);
				}
				else if ( editSeamCells.isSelected() )
				{
					// Start seam cell editing:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.EditSeamCells, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openSeamCells();
					}
					else
					{
						editMode = EditSeamCells;
						openSeamCells();
					}
				}
				else if ( editLattice.isSelected() )
				{
					// start lattice editing:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.EditLattice, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openLattice();
					}
					else
					{
						editMode = EditLattice;
						openLattice();
					}
				}
				else if ( checkSeamCells.isSelected() )
				{
					// start lattice editing:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.CheckSeam, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.checkSeam();
					}
					else
					{
						editMode = CheckSeam;
						checkSeam();
					}
				}
				else if ( editAnnotations1.isSelected() )
				{
					// start annotation editing:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.EditAnnotations1, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openAnnotations(0);
					}
					else
					{
						editMode = EditAnnotations1;
						openAnnotations(0);
					}
				}
				else if ( editAnnotations2.isSelected() )
				{
					// start annotation editing:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.EditAnnotations2, includeRange, imageIndex, baseFileDir2, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openAnnotations(1);
					}
					else
					{
						editMode = EditAnnotations2;
						openAnnotations(1);
					}
				}
				else if ( reviewResults.isSelected() )
				{
					// start viewing untwisted results:
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.ReviewResults, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openStraightened();
					}
					else
					{
						editMode = ReviewResults;
						openStraightened();
					}
				}
			}

			baseFileNameText.setEnabled( !createAnimation.isSelected() );
			rangeFusionText.setEnabled( !createAnimation.isSelected() );
		}
		else
		{
			// Edit mode, next and back open the next or previous image in the
			// sequence to edit and opens the associated VOIs.
			if ( command.equals("next") )
			{
				if ( editMode == EditSeamCells )
				{
					if ( (wormData != null) && !wormData.checkSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate an even number of seam cells (optional: one nose point; optional: highlight first pair and last pair)");
						return;
					}
					if ( (wormData != null) && !wormData.checkHeadSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the first pair");
						return;
					}
					if ( (wormData != null) && !wormData.checkTailSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the last pair");
						return;
					}
				}
				voiManager.clear3DSelection();
				save();
				imageIndex++;

				if ( editMode == EditSeamCells )
				{
					openSeamCells();
				}
				else if ( editMode == EditLattice )
				{
					openLattice();
				}
				else if ( editMode == CheckSeam )
				{
					checkSeam();
				}
				else if ( editMode == EditAnnotations1 )
				{
					openAnnotations(0);
				}
				else if ( editMode == EditAnnotations2 )
				{
					openAnnotations(1);
				}
				else if ( editMode == ReviewResults )
				{
					openStraightened();
				}
			}
			else if ( command.equals("back") )
			{
				if ( editMode == EditSeamCells )
				{
					if ( (wormData != null) && !wormData.checkSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate an even number of seam cells (optional: one nose point; optional: highlight first pair and last pair)");
						return;
					}
					if ( (wormData != null) && !wormData.checkHeadSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the first pair");
						return;
					}
					if ( (wormData != null) && !wormData.checkTailSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the last pair");
						return;
					}
				}
				voiManager.clear3DSelection();
				save();
				imageIndex--;
				if ( editMode == EditSeamCells )
				{
					openSeamCells();
				}
				else if ( editMode == EditLattice )
				{
					openLattice();
				}
				else if ( editMode == CheckSeam )
				{
					checkSeam();
				}
				else if ( editMode == EditAnnotations1 )
				{
					openAnnotations(0);
				}
				else if ( editMode == EditAnnotations2 )
				{
					openAnnotations(1);
				}
				else if ( editMode == ReviewResults )
				{
					openStraightened();
				}
			}
			// Closes the editing:
			else if (command.equals("done"))
			{			
				backNextPanel.remove(displayModel);
				if ( editMode == EditSeamCells )
				{
					if ( (wormData != null) && !wormData.checkSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate an even number of seam cells (optional: one nose point; optional: highlight first pair and last pair)");
						return;
					}
					if ( (wormData != null) && !wormData.checkHeadSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the first pair");
						return;
					}
					if ( (wormData != null) && !wormData.checkTailSeamCells(voiManager.getAnnotations()) )
					{
						MipavUtil.displayError( "Please designate two seam cells as the last pair");
						return;
					}
				}
				if ( voiManager != null )
				{
					voiManager.clear3DSelection();
				}
				save();
				if ( parent != null )
				{
					parent.enableNext( editMode );
					setVisible(false);
					dispose();
				}
				else
				{
					enableNext(editMode);
				}		
				if ( annotationPanelUI != null ) {
					tabbedPane.remove(annotationPanelUI.getAnnotationsPanel());
				}
			}
			// Enables user to generate a new lattice (when none of the automatic ones match well)
			else if (command.equals("newLattice") )
			{
				if ( voiManager != null )
				{
					voiManager.clear3DSelection();
					voiManager.setLattice(new VOIVector());
					voiManager.editLattice();
				}
				latticeSelectionPanel.removeAll();
				latticeSelectionPanel.add(newLatticeButton);
				latticeSelectionPanel.add(flipLatticeButton);
				latticeSelectionPanel.add(displayModel);
				displayModel.setSelected(false);
				this.validate();
			}
			else if ( command.equals("flipLattice" ) )
			{
				if ( voiManager != null )
				{
					voiManager.flipLattice();
				}				
			}
			else if ( command.equals("displayModel") )
			{
				if ( voiManager != null )
				{
					voiManager.showModel( displayModel.isSelected() );
					volumeRenderer.updateVOIs();
				}
			}
			if ( includeRange != null )
			{
				imageIndex = Math.min( includeRange.size() - 1, imageIndex );
				imageIndex = Math.max( 0, imageIndex );

				backButton.setEnabled( imageIndex > 0 );
				nextButton.setEnabled( imageIndex < (includeRange.size() - 1));
			}
			if ( latticeChoices != null )
			{
				if ( voiManager != null )
				{
					voiManager.clear3DSelection();
				}
				for ( int i = 0; i < latticeChoices.length; i++ )
				{
					if ( (source == latticeChoices[i]) && (latticeChoices[i].isSelected()) )
					{
						if ( voiManager != null )
						{
							//							System.err.println( "Switching lattices to " + i );
							voiManager.setLattice(potentialLattices[i]);
						}
					}
				}
			}
		}
		if (command.equals("close"))
		{			
			setVisible(false);
	        if ( ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
	                && ViewUserInterface.getReference().isPlugInFrameVisible() )
	        {
                System.exit(0);
            } else {
                dispose();
            }
		}
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.model.algorithms.AlgorithmInterface#algorithmPerformed(gov.nih.mipav.model.algorithms.AlgorithmBase)
	 */
	public void algorithmPerformed(AlgorithmBase algorithm)
	{       
		if ( (annotationList != null) && (annotationNames != null) && (triVolume != null) )
		{
			triVolume.addVOIS( annotationList, annotationNames );
			triVolume.displayAnnotationSpheres();
			triVolume.display3DWindowOnly();
			return;
		}

		float min = (float) wormImage.getMin();
		float max = (float) wormImage.getMax();
		TransferFunction kTransfer = new TransferFunction();
		kTransfer.removeAll();
		kTransfer.addPoint(min, 255);
		kTransfer.addPoint((min + ((max - min) / 3.0f)), 255 * 0.67f);
		kTransfer.addPoint((min + ((max - min) * 2.0f / 3.0f)), 255 * 0.333f);
		kTransfer.addPoint(max, 0);
		volumeRenderer.getVolumeImage().UpdateImages(kTransfer, 0, null);
		updateClipPanel();

//		System.err.println( "algorithmPerformed" );
		voiManager = new VOILatticeManagerInterface( null, volumeImage.GetImage(), null, 0, true, null );
		volumeRenderer.setVOILatticeManager(voiManager);
		if ( (editMode == EditSeamCells) || (editMode == EditAnnotations1) || (editMode == EditAnnotations2) || (editMode == CheckSeam) )
		{
			if ( annotations != null )
			{
				if ( annotations.size() > 0 )
				{
					voiManager.setAnnotations(annotations);
					if ( (editMode == EditAnnotations1) || (editMode == EditAnnotations2) || (editMode == CheckSeam) )
					{
						if ( finalLattice != null ) {
							VOIVector latticeVector = new VOIVector();
							latticeVector.add(finalLattice);
							voiManager.setLattice(latticeVector);
						}
					}

					for (int i = 0; i < annotations.elementAt(0).getCurves().size(); i++)
					{
						final VOIText text = (VOIText) annotations.elementAt(0).getCurves().elementAt(i);
						text.createVolumeVOI( volumeImage, volumeRenderer.getTranslate() );    			
					}
				}
			}
			voiManager.editAnnotations(editMode == EditSeamCells);
			voiManager.colorAnnotations(editMode == EditSeamCells);
			// initialize the display panel for editing / displaying annotations:
			initDisplayAnnotationsPanel();
		}
		else if ( editMode == EditLattice )
		{
			if ( potentialLattices != null )
			{
				if ( potentialLattices[0] != null )
				{
					voiManager.setLattice(potentialLattices[0]);
				}
			}
			voiManager.editLattice();
		}
		else if ( editMode == ReviewResults )
		{
			volumeRenderer.resetAxisX();
		}

		volumeRenderer.displayVolumeSlices(false);
		volumeRenderer.displayVolumeRaycast(true);
		volumeRenderer.displayVOIs(true);
		volumeRenderer.setVolumeBlend(.8f);
		volumeRenderer.setABBlend(.8f);
//		volumeRenderer.initLUT();
	}
	
	public void dispose()
	{
		super.dispose();
		if ( annotationList != null )
		{
			annotationList.clear();
			annotationList = null;
		}
		if ( annotationNames != null )
		{
			annotationNames.clear();
			annotations = null;
		}
		if ( includeRange != null )
		{
			includeRange.clear();
			includeRange = null;
		}
		if ( triVolume != null )
		{
			triVolume.disposeLocal(false);
			triVolume = null;
		}
		if ( volumeImage != null )
		{
			volumeImage.dispose();
			volumeImage = null;
		}
		if ( voiManager != null )
		{
			voiManager.disposeLocal(false);
			voiManager = null;
		}
		if ( volumeRenderer != null )
		{
			volumeRenderer.dispose();
			volumeRenderer = null;
		}
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}		
	}
	
	/**
	 * Called when the user is done viewing the volumes and editing the seam cells, lattice, or annotations.
	 * The next step in the straightening process is automatically enabled and selected.
	 * @param mode
	 */
	public void enableNext( int mode )
	{
		if ( volumeRenderer != null )
		{
			volumeRenderer.dispose();
			volumeRenderer = null;
		}
		if ( voiManager != null )
		{
			voiManager = null;
		}
		gpuPanel.removeAll();
		if ( volumeImage != null )
		{
			volumeImage.dispose();
			volumeImage = null;
		}
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}
		if ( wormData != null )
		{
			wormData.dispose();
			wormData = null;
		}
		if ( mode == EditSeamCells )
		{
			buildLattice.setSelected(true);
		}
		else if ( mode == EditLattice )
		{
			checkSeamCells.setSelected(true);
		}
		else if ( mode == CheckSeam )
		{
			editAnnotations1.setSelected(true);
		}
		else if ( mode == EditAnnotations1 )
		{
			if ( editAnnotations2.isEnabled() )
			{
				editAnnotations2.setSelected(true);
			}
			else
			{
				latticeStraighten.setSelected(true);				
			}
		}
		else if ( mode == EditAnnotations2 )
		{
			latticeStraighten.setSelected(true);
		}
		else if ( mode == ReviewResults )
		{
			calcMaxProjection.setSelected(true);
		}
		imageIndex = 0;
		editMode = EditNONE;
		batchProgress.setValue(0);
		batchProgress.update(batchProgress.getGraphics());
		volumePanel.setVisible(false);
		tabbedPane.setVisible(false);
		startButton.setEnabled(true);
		if ( latticeSelectionPanel != null )
		{
			latticeSelectionPanel.setVisible(false);
		}
		pack();
	}

	/* (non-Javadoc)
	 * @see java.beans.PropertyChangeListener#propertyChange(java.beans.PropertyChangeEvent)
	 */
	public void propertyChange(PropertyChangeEvent event) {
		String propertyName = event.getPropertyName();
		if ( propertyName.equals("Opacity") )
		{
			final TransferFunction kTransfer = volOpacityPanel.getCompA().getOpacityTransferFunction();
			volumeImage.UpdateImages(kTransfer, 0, null);
		}
	}

	@Override
	public void setSlice(int slice) {}
	
	@Override
	public void setTimeSlice(int tSlice) {}
	@Override
	public boolean updateImageExtents() {
		return false;
	}
	@Override
	public boolean updateImages() {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(volumeImage.getLUT());
			if ( (volumeRenderer != null) && (volumeImage.getLUT() instanceof ModelRGB) )
			{
				volumeRenderer.setRGBTA( (ModelRGB)volumeImage.getLUT() );
			}
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(boolean)
	 */
	public boolean updateImages(boolean flag) {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(volumeImage.getLUT());
			if ( (volumeRenderer != null) && (volumeImage.getLUT() instanceof ModelRGB) )
			{
				volumeRenderer.setRGBTA( (ModelRGB)volumeImage.getLUT() );
			}
		}
		return false;
	}
	
	/* (non-Javadoc)
	 * @see gov.nih.mipav.view.ViewImageUpdateInterface#updateImages(gov.nih.mipav.model.structures.ModelLUT, gov.nih.mipav.model.structures.ModelLUT, boolean, int)
	 */
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(LUTa);
		}
		return false;
	}
	
    @Override
	public void windowActivated(WindowEvent e) {}

	@Override
	public void windowClosed(WindowEvent e) {}

	public void windowClosing(final WindowEvent event)
    {
        if ( ViewUserInterface.getReference() != null && !ViewUserInterface.getReference().isAppFrameVisible()
                && ViewUserInterface.getReference().isPlugInFrameVisible() )
        {
            System.exit(0);
        } else {
            dispose();
        }
    }

	@Override
	public void windowDeactivated(WindowEvent e) {}
	
	@Override
	public void windowDeiconified(WindowEvent e) {}

	@Override
	public void windowIconified(WindowEvent e) {}

	@Override
	public void windowOpened(WindowEvent e) {}

	/**
	 * Opens the current image and annotation VOIs for viewing/editing.
	 */
	protected void openAnnotations( int whichImage )
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				backNextPanel.remove(displayModel);
				backNextPanel.add(displayModel);
				displayModel.setSelected(false);
				
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				File voiFile2 = new File(baseFileDir2 + File.separator + fileName);
				if ( openImages( voiFile, null, fileName ) )
				{
					// open default file and get the lattice:
					wormData = new WormData(wormImage);
					finalLattice = wormData.readFinalLattice();
					if ( whichImage == 0 )
					{
						// continue with the default file:
						if ( finalLattice != null ) 
						{
							wormImage.registerVOI( finalLattice );
						}		

						if ( annotations != null )
						{
							annotations.clear();
							annotations = null;
						}
						annotations = new VOIVector();
						VOI markers = wormData.getMarkerAnnotations();
						if ( markers != null )
						{
							annotations.add( markers );
							wormImage.registerVOI( markers );
						}
						if ( (annotations.size() > 0) && (voiManager != null) )
						{
							initDisplayAnnotationsPanel();
							
							voiManager.setAnnotations(annotations);
							voiManager.editAnnotations(false);
							VOIVector latticeVector = new VOIVector();
							latticeVector.add(finalLattice);
							voiManager.setLattice(latticeVector);
						}
					}
					else
					{
						// open the second image and set the lattice and markers:
						if ( openImages( voiFile2, null, fileName ) )
						{
							wormData = new WormData(wormImage);
							wormImage.registerVOI( finalLattice );

							if ( annotations != null )
							{
								annotations.clear();
								annotations = null;
							}
							annotations = new VOIVector();
							VOI markers = wormData.getMarkerAnnotations();
							if ( markers != null )
							{
								annotations.add( markers );
								wormImage.registerVOI( markers );
							}
							if ( (annotations.size() > 0) && (voiManager != null) )
							{
								initDisplayAnnotationsPanel();
								
								voiManager.setAnnotations(annotations);
								voiManager.editAnnotations(false);
								VOIVector latticeVector = new VOIVector();
								latticeVector.add(finalLattice);
								voiManager.setLattice(latticeVector);
							}
						}
					}
				}
			}
		}
	}


	/**
	 * Opens the current image for viewing. If this is the fist image the volume renderer is created and initialized.
	 * Updates the volume renderer and the histogram / LUT and opacity panels with the new image.
	 * @param imageFile image FIle to open
	 * @param fileName file name
	 * @return true if the file exists.
	 */
	protected boolean openImages( File imageFile, File imageFile2, String fileName )
	{
		if ( imageFile.exists() )
		{
			int[] previousExtents = null;
			//					System.err.println( fileName );
			gpuPanel.setBorder(JDialogBase.buildTitledBorder(fileName));
			FileIO fileIO = new FileIO();
			if ( wormImage != null ) {
				previousExtents = new int[]{wormImage.getExtents()[0], wormImage.getExtents()[1], wormImage.getExtents()[2]};
				wormImage.disposeLocal();
				wormImage = null;
			}
			wormImage = fileIO.readImage(fileName, imageFile.getParent() + File.separator, false, null); 
			wormImage.calcMinMax();     
			
			if ( thresholdImageCheck.isSelected() && ((imageFile2 == null) || ((imageFile2 != null) && !imageFile2.exists())) )
			{
				for ( int i = 0; i < wormImage.getDataSize(); i++ )
				{
					if ( wormImage.getFloat(i) > threshold )
					{
						wormImage.set(i, threshold);
					}
				}
				wormImage.calcMinMax();     
			}
			
			float[] res = wormImage.getResolutions(0);
			res[0] = res[2]; 
			res[1] = res[2]; 
			int[] units = wormImage.getUnitsOfMeasure();
			units[0] = units[2];
			units[1] = units[2];
			FileInfoBase[] fileInfo = wormImage.getFileInfo();
			for ( int i = 0; i < fileInfo.length; i++ )
			{
				fileInfo[i].setResolutions(res);
	            fileInfo[0].setUnitsOfMeasure(units);
			}
			ModelImage secondImage = null;
			if ( (imageFile2 != null) && imageFile2.exists() )
			{
				secondImage = fileIO.readImage(fileName, imageFile2.getParent() + File.separator, false, null); 
				secondImage.calcMinMax();     
				res = secondImage.getResolutions(0);
				res[0] = res[2]; 
				res[1] = res[2]; 
				units = secondImage.getUnitsOfMeasure();
				units[0] = units[2];
				units[1] = units[2];
				fileInfo = secondImage.getFileInfo();
				for ( int i = 0; i < fileInfo.length; i++ )
				{
					fileInfo[i].setResolutions(res);
		            fileInfo[0].setUnitsOfMeasure(units);
				}
			}
			if ( secondImage != null )
			{
				if ( thresholdImageCheck.isSelected() )
				{
					for ( int i = 0; i < wormImage.getDataSize(); i++ )
					{
						if ( wormImage.getFloat(i) > threshold )
						{
							wormImage.set(i, threshold);
						}
						if ( secondImage.getFloat(i) > threshold )
						{
							secondImage.set(i, threshold);
						}
					}
					wormImage.calcMinMax();     
					secondImage.calcMinMax();     
				}
				
				ModelImage displayImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, wormImage.getExtents(),
						JDialogBase.makeImageName(wormImage.getImageName(), "_rgb"));
				JDialogBase.updateFileInfo(wormImage, displayImage);

                // Make algorithm
				ModelImage blank = new ModelImage(ModelImage.SHORT, wormImage.getExtents(), JDialogBase.makeImageName(wormImage.getImageName(), ""));
				AlgorithmRGBConcat mathAlgo = new AlgorithmRGBConcat(secondImage, wormImage, blank, displayImage, true, false, 255, true, true);
				mathAlgo.run();
				
				ModelImage.saveImage(displayImage, displayImage.getImageName(), imageFile.getParent() + File.separator);
				wormImage.disposeLocal(false);
				secondImage.disposeLocal(false);
				blank.disposeLocal(false);
				wormImage = displayImage;
			}
			
			
			if ( previousExtents == null )
			{
				volumeImage = new VolumeImage(false, wormImage, "", null, 0, false);
				updateHistoLUTPanels();

				volumeRenderer = new VolumeTriPlanarRender(volumeImage);
				volumeRenderer.addConfiguredListener(this);
				gpuPanel.add(volumeRenderer.GetCanvas(), BorderLayout.CENTER);
				gpuPanel.setVisible(true);
				tabbedPane.setVisible(true);
				volumePanel.setVisible(true);
				pack();				
			}
			else
			{
				if ( voiManager != null )
				{
					voiManager.setImage(wormImage, null);
				}

				boolean updateRenderer = (wormImage.getExtents()[0] != previousExtents[0]) || 
						(wormImage.getExtents()[1] != previousExtents[1]) ||
						(wormImage.getExtents()[2] != previousExtents[2]);

				volumeImage.UpdateData(wormImage, updateRenderer);
				updateHistoLUTPanels();

				if ( updateRenderer )
				{
					volumeRenderer.resetAxis();
					volumeRenderer.reCreateScene(volumeImage);
				}
				if ( !volumeRenderer.isVisible() )
				{
					volumeRenderer.setVisible(true);
				}
				updateClipPanel();
				if ( !tabbedPane.isVisible() || !volumePanel.isVisible() )
				{
					tabbedPane.setVisible(true);
					volumePanel.setVisible(true);
					pack();									
				}
			}
			
			
			return true;
		}
		return false;
	}
	
	/**
	 * Opens the current image and lattice for viewing/editing.
	 */
	protected void openLattice()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				File voiFile2 = new File(baseFileDir2 + File.separator + fileName);
				if ( openImages( voiFile, voiFile2, fileName ) )
				{
					wormImage.setImageName( wormImage.getImageName().replace("_rgb", ""));
					if ( potentialLattices != null )
					{
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							if (potentialLattices[i] != null )
							{
								potentialLattices[i].clear();
							}
						}
						potentialLattices = null;
					}	
					wormData = new WormData(wormImage);
					latticeSelectionPanel.removeAll();
					latticeSelectionPanel.setVisible(false);

					int latticeIndex = -1;
					VOI finalLattice = wormData.readFinalLattice();
					if ( finalLattice != null )
					{
						if ( potentialLattices == null )
						{
							potentialLattices = new VOIVector[1];
						}
						if ( potentialLattices[0] == null )
						{
							potentialLattices[0] = new VOIVector();
						}
						potentialLattices[0].add(finalLattice);
						latticeSelectionPanel.add(latticeChoices[0]);
						latticeIndex = 0;
					}
					if ( latticeIndex == -1 )
					{
						potentialLattices = wormData.readAutoLattice();
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							if ( potentialLattices[i] != null )
							{
								if ( potentialLattices[i].size() != 0 )
								{
									latticeSelectionPanel.add(latticeChoices[i]);
									if ( latticeIndex == -1 )
									{
										latticeIndex = i;
									}
								}
							}
						}
					}
					latticeSelectionPanel.add(newLatticeButton);
					latticeSelectionPanel.add(flipLatticeButton);
					latticeSelectionPanel.add(displayModel);
					displayModel.setSelected(false);
					latticeSelectionPanel.setVisible(true);
					if ( voiManager != null )
					{
						if ( latticeIndex != -1 )
						{
							voiManager.setLattice(potentialLattices[latticeIndex]);
							latticeChoices[latticeIndex].setSelected(true);
						}
						voiManager.editLattice();
					}
				}
			}
		}
	}
	
	/**
	 *  Opens the current image and seam cells for viewing/editing.
	 */
	protected void openSeamCells()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( openImages( voiFile, null, fileName ) )
				{
					wormData = new WormData(wormImage);
					wormData.readSeamCells();				
					
					if ( annotations != null )
					{
						annotations.clear();
						annotations = null;
					}
					annotations = new VOIVector();
					annotations.add( wormData.getSeamAnnotations() );
					wormImage.registerVOI( wormData.getSeamAnnotations() );

					if ( (annotations.size() > 0) && (voiManager != null) )
					{
						initDisplayAnnotationsPanel();
						
						voiManager.setAnnotations(annotations);
						voiManager.editAnnotations(true);
						voiManager.colorAnnotations(true);
					}
				}
			}
		}
	}
	
	/**
	 * Opens the current volume for viewing including the straightened annotations and lattice.
	 */
	protected void openStraightened()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight.tif";
				String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
				String subDirNameResults = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_results" + File.separator;
				File voiFile = new File(baseFileDir + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
				File voiFile2 = new File(baseFileDir2 + File.separator + subDirName + subDirNameResults + PlugInAlgorithmWormUntwisting.outputImages + File.separator + imageName);
				if ( openImages( voiFile, voiFile2, imageName ) )
				{			
					wormData = new WormData(wormImage);
					wormData.openStraightLattice();
					wormData.openStraightAnnotations();
					wormData.openStraightSeamCells();
					if ( voiFile2 != null )
					{
						wormData.openStraightAnnotations(voiFile2.getParentFile().getParent());
					}
					
					if ( volumeRenderer != null )
					{
						volumeRenderer.resetAxisX();
					}
				}
			}
		}
	}

	/**
	 * Displays the annotation animation visualization framework.
	 */
	private void annotationAnimationFromSpreadSheet()
	{
		int[] extents = new int[3];
		VOIVector tempList = new VOIVector();
		Vector< int[] > timesList = new Vector< int[] >();
		ViewJProgressBar progress = new  ViewJProgressBar( "Generating Animation", "", 0, 100, false);
        MipavUtil.centerOnScreen(progress);

		String inputDirName = baseFileDir + File.separator;
		//		System.err.println( inputDirName );
		final File inputFileDir = new File(inputDirName);

		Vector3f min = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
		Vector3f max = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
//		int timeCount, maxTimeCount = -1, minTimeCount = Integer.MAX_VALUE;
//		int maxIndex = -1;
		int fileIndex = 0;
		int startTime = -1;
		int endTime = -1;
		if (inputFileDir.exists() && inputFileDir.isDirectory()) {
	        progress.setVisible(true);
			String[] list = inputFileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File annotationFile = new File( inputFileDir + File.separator + list[i] );
				if ( annotationFile.isDirectory() )
				{
					continue;
				}
				if ( !list[i].endsWith(".csv") )
				{
					continue;
				}
				int index = list[i].indexOf(".");
				String annotationName = new String(list[i]);
				if ( index != -1 )
				{
					annotationName = annotationName.substring(0, index);
				}
				//        		System.err.println( annotationName );

//				timeCount = 0;
				VOI annotation = new VOI( (short)i, annotationName, VOI.ANNOTATION, 0 );
				Vector<Integer> times = new Vector<Integer>();
				FileReader fr;
				try {
					int red = 255;
					int green = 255;
					int blue = 255;
					fr = new FileReader(annotationFile);
					BufferedReader br = new BufferedReader(fr);

					String line = br.readLine();
					line = br.readLine();
					//    				System.err.println(line);
					String[] parsed = line.split( "," );
					if ( parsed.length > 0 )
					{
						if ( parsed[0].equals("color") )
						{
							red    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Integer.valueOf( parsed[1] ) : 0 : 0; 
							green  = (parsed.length > 2) ? (parsed[2].length() > 0) ? Integer.valueOf( parsed[2] ) : 0 : 0; 
							blue   = (parsed.length > 3) ? (parsed[3].length() > 0) ? Integer.valueOf( parsed[3] ) : 0 : 0; 

							line = br.readLine();
						}
					}
					while ( line != null )
					{
						//        				System.err.println(line);
						int time = -1;
						float x = 0, y = 0, z = 0;
						parsed = line.split( "," );
						time = (parsed.length > 0) ? (parsed[0].length() > 0) ? Integer.valueOf( parsed[0] ) : -1 : -1; 
						x    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
						y    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
						z    = (parsed.length > 3) ? (parsed[3].length() > 0) ? Float.valueOf( parsed[3] ) : 0 : 0; 

						if ( time != -1 )
						{
							if ( startTime > time )
							{
								startTime = time;
							}
							if ( endTime < time )
							{
								endTime = time;
							}
						}
						if ( (time != -1) && (z >= 0) && (parsed.length > 3) )
						{
							VOIText text = new VOIText();
							text.setText( list[i] );
							text.setColor( new Color(red, green, blue) );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x, y, z ) );
							text.setText(annotationName);
							annotation.getCurves().add(text);

							times.add( time );
//							timeCount++;

							min.min( text.elementAt(0) );
							max.max( text.elementAt(0) );

//							System.err.println( annotationName + "  " + time + "   " + x + "  " + y + "  " + z );
							//    						if ( text.elementAt(0).Z < 0 )
							//    						{
							//    							System.err.println(list[i] );
							//    						}
						}
						line = br.readLine();
					}
					br.close();
					fr.close();

					tempList.add(annotation);

					int[] timesArray = new int[times.size()];
					for ( int j = 0; j < times.size(); j++ )
					{
						timesArray[j] = times.elementAt(j);
					}
					timesList.add( timesArray );
				} catch (FileNotFoundException e) {
					MipavUtil.displayError("Error reading file: " + inputDirName + " " + e.getCause() );
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					MipavUtil.displayError("Error reading file: " + inputDirName + " " + e.getCause() );
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
//				if ( timeCount < minTimeCount )
//				{
//					minTimeCount = timeCount;
//					maxIndex = fileIndex;
//				}
//				if ( timeCount > maxTimeCount )
//				{
//					maxTimeCount = timeCount;
//					maxIndex = fileIndex;
//				}
				fileIndex++;
				progress.updateValueImmed((int) (100 * (float)fileIndex/(float)list.length));
			}
		}

		//        System.err.println( minTimeCount + " " + maxTimeCount + " " + (minTimeCount == maxTimeCount ) );

		//        System.err.println( timesList.size() + " " + tempList.size() );
//		int[] times = timesList.elementAt( maxIndex );
//		VOI curve = tempList.elementAt( maxIndex );
		//		for ( int j = 0; j < times.length; j++ )
		//		{
		//			System.err.println( curve.getName() + " " + times[j] );
		//		}


		annotationList = new VOIVector();
		for ( int i = startTime; i <= endTime; i++ )
		{
//			int timeStep = times[i];
			VOI annotation = new VOI( (short)i, "time" + i, VOI.ANNOTATION, 0 );

			//			System.err.print( timeStep );
			for ( int j = 0; j < timesList.size(); j++ )
			{
				int[] currentTimes = timesList.elementAt(j);
				for ( int k = 0; k < currentTimes.length; k++ )
				{
					if ( i == currentTimes[k] )
					{
						VOIText text = new VOIText(tempList.elementAt(j).getCurves().elementAt(k));

						text.setText( ((VOIText)tempList.elementAt(j).getCurves().elementAt(k)).getText() );
						text.setColor( ((VOIText)tempList.elementAt(j).getCurves().elementAt(k)).getColor() );
						annotation.getCurves().add( text );

						//        				System.err.print( " " + text.getText() );
						break;
					}
				}
			}
			//			System.err.println( "" );  		


			annotationList.add(annotation);
		}
		tempList = null;
		timesList = null;


		int maxCount = -1;
		int maxCountIndex = -1;
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			if ( annotationList.elementAt(i).getCurves().size() > maxCount )
			{
				maxCount = annotationList.elementAt(i).getCurves().size();
				maxCountIndex = i;
			}
		}
		annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.elementAt(maxCountIndex).getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotationList.elementAt(maxCountIndex).getCurves().elementAt(i);
			annotationNames.add( new String(text.getText()) );
		}

		//		System.err.println( min );
		//		System.err.println( max );

		extents[0] = (int)Math.max( 30, (max.X - min.X) + 10);
		extents[1] = (int)Math.max( 30, (max.Y - min.Y) + 10);
		extents[2] = (int)Math.max( 30, (max.Z - min.Z) + 10);

		ModelImage animationImage = new ModelImage( ModelStorageBase.BOOLEAN, extents, "animationImage" );
		String outputDirName = baseFileDir + File.separator + "animation" + File.separator;
		final File outputFileDir = new File(outputDirName);

		if (outputFileDir.exists() && outputFileDir.isDirectory()) {
			String[] list = outputFileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File lrFile = new File( outputFileDir + list[i] );
				lrFile.delete();
			}
		} else if (outputFileDir.exists() && !outputFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			outputFileDir.mkdir();
		}
		
		progress.setTitle("Creating Animation Viewer..." );
		animationImage.setImageDirectory( outputDirName );		
		triVolume = new VolumeTriPlanarInterface(animationImage, null);
		triVolume.addConfiguredListener(this);

		progress.dispose();
		progress = null;
	}

	/**
	 * Initializes the panels for a non-integrated display. 
	 */
	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - lattice - 2.0");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		dialogGUI = new JDialogStandalonePlugin();
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

		baseFileLocText = gui.buildFileField("Data directory (marker 1): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileLocText2 = gui.buildFileField("Data directory (marker 2): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText2.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		JPanel startPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		startPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		startPanel.add( closeButton );
		startPanel.add(new JPanel());
		//		panel.add(buttonPanel, gbc);

		JPanel optionsPanel = makeOptionsPanel(gui);
		
		ButtonGroup group = new ButtonGroup();
		algorithmsPanel = makeAlgorithmsPanel(gui, group);
		editPanel = makeEditPanel(gui, group);

		choicePanel = new JPanel( new GridLayout(1,2) );
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
		
//		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
//		dialogGUI.getContentPane().add(choicePanel, BorderLayout.CENTER);
//		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);


		setLocation(0, 0);
		pack();
		setResizable(true);

		segmentSeamCells.setSelected(true);
	}

	/**
	 * User-interface initialization. If the UI is integrated all panels are displayed in one window.
	 * Otherwise the UI is divided into volume display and separate UI panels.
	 * @param integrated
	 */
	private void init(boolean integrated)
	{
		if ( !integrated )
		{
			init();
			return;
		}
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - lattice - 2.0");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}

		dialogGUI = new JDialogStandalonePlugin();
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

		baseFileLocText = gui.buildFileField("Data directory (marker 1): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileLocText2 = gui.buildFileField("Data directory (marker 2): ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText2.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		JPanel startPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		startPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		startPanel.add( closeButton );
		startPanel.add(new JPanel());

		JPanel optionsPanel = makeOptionsPanel(gui);
		
		ButtonGroup group = new ButtonGroup();
		algorithmsPanel = makeAlgorithmsPanel(gui, group);
		editPanel = makeEditPanel(gui, group);

		choicePanel = new JPanel( new GridLayout(1,2) );
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
		
//		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
//		dialogGUI.getContentPane().add(choicePanel, BorderLayout.CENTER);
//		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

//		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);

		lutPanel = new JPanel();
		opacityPanel = new JPanel();
		clipPanel = new JPanel();
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab("LUT", null, lutPanel);
		tabbedPane.addTab("Opacity", null, opacityPanel);
		tabbedPane.addTab("Clip", null, clipPanel);
		tabbedPane.setVisible(false);
		
		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.add( dialogGUI.getContentPane(), BorderLayout.NORTH );
		leftPanel.add( tabbedPane, BorderLayout.CENTER );


		volumePanel = initGPUPanel(EditNONE);
		volumePanel.setVisible(false);
		
		
		JPanel integratedPanel = new JPanel( new BorderLayout() );
		integratedPanel.add( leftPanel, BorderLayout.WEST );
		integratedPanel.add( volumePanel, BorderLayout.EAST );
		getContentPane().add(integratedPanel, BorderLayout.CENTER);
        
		setLocation(0, 0);
		pack();
		setResizable(true);

		segmentSeamCells.setSelected(true);
	}

	/**
	 * Initializes volume rendering with the GPU.
	 * @param editMode
	 */
	private void init( int editMode )
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans");
		try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
		}
		
		getContentPane().add(initGPUPanel(editMode), BorderLayout.CENTER);

		pack();
		setResizable(true);

	}

	/**
	 * Sets up the GPU volume display panel, with the 'back' and 'next' buttons for
	 * going through the images and editing the seam cells or lattices.
	 * @param editMode when the edit mode is for editing lattices up to 5 lattice options are shown to the user.
	 * @return
	 */
	private Container initGPUPanel( int editMode )
	{
		MipavInitGPU.InitGPU();

		dialogGUI = new JDialogStandalonePlugin();
		GuiBuilder gui = new GuiBuilder(dialogGUI);

		backNextPanel = new JPanel();
		backButton = gui.buildButton("back");
		backButton.addActionListener(this);
		backButton.setActionCommand("back");
		backButton.setVisible(true);
		backButton.setEnabled(false);
		backNextPanel.add( backButton );

		nextButton = gui.buildButton("next");
		nextButton.addActionListener(this);
		nextButton.setActionCommand("next");
		nextButton.setVisible(true);
		nextButton.setEnabled(true);
		backNextPanel.add( nextButton );

		doneButton = gui.buildButton("done");
		doneButton.addActionListener(this);
		doneButton.setActionCommand("done");
		doneButton.setVisible(true);
		doneButton.setEnabled(true);
		backNextPanel.add( doneButton );

		ButtonGroup latticeGroup = new ButtonGroup();
		latticeSelectionPanel = new JPanel();
		latticeChoices = new JRadioButton[5];
		for ( int i = 0; i < latticeChoices.length; i++ )
		{
			latticeChoices[i] = gui.buildRadioButton("lattice_" + (i+1), i==0 );
			latticeChoices[i].addActionListener(this);
			latticeGroup.add(latticeChoices[i]);
		}
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
		displayModel.setVisible(true);
		displayModel.setEnabled(true);
		latticeSelectionPanel.add(displayModel);
		
		backNextPanel.add( latticeSelectionPanel );
		latticeSelectionPanel.setVisible(false);


		gpuPanel = new JPanel(new BorderLayout());
		final int imagePanelWidth = (int) (Toolkit.getDefaultToolkit().getScreenSize().width * 0.5f);
		final int imagePanelHeight = (int) (Toolkit.getDefaultToolkit().getScreenSize().height * 0.5f);

		gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
		gpuPanel.setMinimumSize(new Dimension(250, 250));
		gpuPanel.setBorder(JDialogBase.buildTitledBorder("Volume Display"));

		dialogGUI.getContentPane().add(gpuPanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(backNextPanel, BorderLayout.SOUTH);

		return dialogGUI.getContentPane();
	}
	
	

	/**
	 * The annotations panel is added to the VolumeTriPlanarInterface for display.
	 */
	private void initDisplayAnnotationsPanel( )
	{		
		if ( annotationPanelUI == null )
		{
			annotationPanelUI = new JPanelAnnotations(voiManager, volumeImage.GetImage());
		}
		annotationPanelUI.initDisplayAnnotationsPanel(voiManager, volumeImage.GetImage(), annotations.elementAt(0));
		tabbedPane.addTab("Annotation", null, annotationPanelUI.getAnnotationsPanel());
		pack();
	}

	/**
	 * Builds the algorithms panel for automatic seam-cell detection, automatic lattice building, straightening, etc.
	 * Sets up the buttons and return the panel.
	 * @param gui
	 * @param group
	 * @return the user-interface panel.
	 */
	private JPanel makeAlgorithmsPanel(GuiBuilder gui, ButtonGroup group)
	{
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
//		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Batch Algorithms"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		segmentSeamCells = gui.buildRadioButton("1). segment seam cells", true );
		segmentSeamCells.addActionListener(this);
		panel.add(segmentSeamCells.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		buildLattice = gui.buildRadioButton("3). generate lattices", false );
		buildLattice.addActionListener(this);
		panel.add(buildLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		latticeStraighten = gui.buildRadioButton("6). straighten", false );
		latticeStraighten.addActionListener(this);
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		calcMaxProjection = gui.buildRadioButton("generate maximum intensity projection animation", false );
		calcMaxProjection.addActionListener(this);
		panel.add(calcMaxProjection.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		resliceRotate = gui.buildRadioButton("Reslice and rotate", false );
		resliceRotate.addActionListener(this);
		panel.add(resliceRotate.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		batchProgress = new JProgressBar(0,100);
		panel.add(batchProgress, gbc);

		group.add(segmentSeamCells);
		group.add(buildLattice);
		group.add(latticeStraighten);
		group.add(calcMaxProjection);
		group.add(resliceRotate);
		
		return panel;
	}

	/**
	 * Generates the panel for editing seam cells, lattices, annotations, inspecting the straightened image results, etc.
	 * @param gui
	 * @param group
	 * @return user-interface panel.
	 */
	private JPanel makeEditPanel(GuiBuilder gui, ButtonGroup group)
	{
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
//		gbc.insets = new Insets(3, 3, 3, 3);
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.gridx = 0;
		gbc.gridy = 0;

		gbc.gridx = 0;
		gbc.gridy = 0;
		JPanel panel = new JPanel(new GridBagLayout());
		panel.setBorder(JDialogBase.buildTitledBorder("Build / Edit"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		editSeamCells = gui.buildRadioButton("2). edit seam cells", false );
		editSeamCells.addActionListener(this);
		editSeamCells.setActionCommand("editSeamCells");
		panel.add(editSeamCells.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		editLattice = gui.buildRadioButton("4a). edit lattice", false );
		editLattice.addActionListener(this);
		editLattice.setActionCommand("editLattice");
		panel.add(editLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		checkSeamCells = gui.buildRadioButton("4b). check seam cells", false );
		checkSeamCells.addActionListener(this);
		checkSeamCells.setActionCommand("checkSeamCells");
		panel.add(checkSeamCells.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		editAnnotations1 = gui.buildRadioButton("5a). add annotations channel 1", false );
		editAnnotations1.addActionListener(this);
		editAnnotations1.setActionCommand("editAnnotations");
		panel.add(editAnnotations1.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		editAnnotations2 = gui.buildRadioButton("5b). add annotations channel 2", false );
		editAnnotations2.addActionListener(this);
		editAnnotations2.setActionCommand("editAnnotations");
		panel.add(editAnnotations2.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		reviewResults = gui.buildRadioButton("7). review straightened results", false );
		reviewResults.addActionListener(this);
		reviewResults.setActionCommand("reviewResults");
		panel.add(reviewResults.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		createAnimation = gui.buildRadioButton("create annotation animation", false );
		createAnimation.addActionListener(this);
		createAnimation.setActionCommand("createAnimation");
		panel.add(createAnimation.getParent(), gbc);
		gbc.gridy++;

		group.add(editSeamCells);
		group.add(editLattice);
		group.add(checkSeamCells);
		group.add(editAnnotations1);
		group.add(editAnnotations2);
		group.add(createAnimation);
		group.add(reviewResults);

		return panel;
	}
	
	private JPanel makeOptionsPanel(GuiBuilder gui)
	{
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.anchor = GridBagConstraints.NORTHWEST;
		gbc.weightx = 1;
//		gbc.insets = new Insets(3, 3, 3, 3);
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
		panel.add(segmentationPaddingText.getParent(), gbc);
		gbc.gridy++;

		thresholdImageCheck = gui.buildCheckBox( "Threshold image (volume viewing)", false);
		panel.add(thresholdImageCheck.getParent(), gbc);
		gbc.gridx++;
		
		thresholdValue = gui.buildField( "value: ", "75" );
		panel.add(thresholdValue.getParent(), gbc);
		gbc.gridx++;
		
		gbc.gridx = 0;
		gbc.gridy++;

		resliceImageCheck = gui.buildCheckBox( "Reslice straightened", true);
		panel.add(resliceImageCheck.getParent(), gbc);
		gbc.gridx++;
		
		resliceX = gui.buildField( "x:", String.valueOf(resliceXValue) );
		panel.add(resliceX.getParent(), gbc);
		gbc.gridx++; gbc.gridx++;
		
		resliceY = gui.buildField( "  y:", String.valueOf(resliceYValue) );
		panel.add(resliceY.getParent(), gbc);
		gbc.gridx++; gbc.gridx++;
		
		resliceZ = gui.buildField( "  z:", String.valueOf(resliceZValue) );
		panel.add(resliceZ.getParent(), gbc);
		gbc.gridx++;
		
		gbc.gridx = 0;
		gbc.gridy++;
		
		return panel;
	}

	/**
	 * Saves seam cells, lattice, or annotations based on the current edit mode.
	 */
	private void save()
	{
		if ( editMode == EditSeamCells )
		{
			saveSeamCells();
		}
		else if ( editMode == EditLattice )
		{
			saveLattice();
		}
		else if ( editMode == CheckSeam )
		{
			saveSeamCells();
		}
		else if ( editMode == EditAnnotations1 )
		{
			saveAnnotations();
		}
		else if ( editMode == EditAnnotations2 )
		{
			saveAnnotations();
		}
	}

	/**
	 * Saves the annotations to the default edited file for the current image.
	 */
	private void saveAnnotations()
	{
		if ( wormImage == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
		if ( wormData == null )
		{
			return;
		}
		if ( voiManager == null )
		{
			return;
		}

		wormData.saveMarkerAnnotations(voiManager.getAnnotations());
		wormData.dispose();
	}

	/**
	 * Saves the lattice to the default edited file for the current image.
	 */
	private void saveLattice()
	{
		if ( wormImage == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
		String imageName = wormImage.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String outputDirectory = new String(wormImage.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + JDialogBase.makeImageName(imageName, "_results") );
		voiManager.saveLattice( outputDirectory + File.separator, PlugInAlgorithmWormUntwisting.editLatticeOutput );

	}
	
	private void checkSeam()
	{		
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( openImages( voiFile, null, fileName ) )
				{
					wormData = new WormData(wormImage);
					wormData.readNamedSeamCells();				
					
					if ( annotations != null )
					{
						annotations.clear();
						annotations = null;
					}
					annotations = new VOIVector();
					annotations.add( wormData.getSeamAnnotations() );
					wormImage.registerVOI( wormData.getSeamAnnotations() );

					if ( (annotations.size() > 0) && (voiManager != null) )
					{
						voiManager.setAnnotations(annotations);
						voiManager.editAnnotations(false);
						voiManager.colorAnnotations(false);
					}
					finalLattice = wormData.readFinalLattice();
					if ( (finalLattice != null) && (voiManager != null) ) 
					{
						VOIVector latticeVector = new VOIVector();
						latticeVector.add(finalLattice);
						voiManager.setLattice( latticeVector );
					}					
				}
			}
		}	
	}

	/**
	 * Saves the seam cells to the default edited file for the current image.
	 */
	private void saveSeamCells()
	{
		if ( wormImage == null )
		{
			return;
		}
		if ( imageIndex >= includeRange.size() )
		{
			return;
		}
		if ( wormData == null )
		{
			return;
		}
		wormData.saveSeamAnnotations( voiManager.getAnnotations(), editMode != CheckSeam );
		wormData.dispose();
	}

	/**
	 * Sets the include range list of file IDs when the user presses 'start'.
	 * @return true if there are files in the list to process.
	 */
	private boolean setVariables()
	{	    
		try {
			paddingFactor = Integer.valueOf(segmentationPaddingText.getText().trim());
		} catch(NumberFormatException e) {
			paddingFactor = 0;
		}
		try {
			minRadius = Integer.valueOf(seamCellMinRadiusText.getText().trim());
		} catch(NumberFormatException e) {
			minRadius = 8;
		}
		try {
			maxRadius = Integer.valueOf(seamCellMaxRadiusText.getText().trim());
		} catch(NumberFormatException e) {
			maxRadius = 25;
		}
		try {
			resliceXValue = Integer.valueOf(resliceX.getText().trim());
		} catch(NumberFormatException e) {
			resliceXValue = 250;
		}
		try {
			resliceYValue = Integer.valueOf(resliceY.getText().trim());
		} catch(NumberFormatException e) {
			resliceYValue = 250;
		}
		try {
			resliceZValue = Integer.valueOf(resliceZ.getText().trim());
		} catch(NumberFormatException e) {
			resliceZValue = 1500;
		}
		try {
			threshold = -1;
			if ( thresholdImageCheck.isSelected() )
			{
				threshold = Integer.valueOf(thresholdValue.getText().trim());
			}
		} catch(NumberFormatException e) {
			threshold = 75;
		}
		
		baseFileName = baseFileNameText.getText();
		baseFileDir = baseFileLocText.getText();
		baseFileDir2 = baseFileLocText2.getText();
		editAnnotations2.setEnabled( !baseFileDir2.isEmpty() );
		
		includeRange = new Vector<Integer>();
		String rangeFusion = rangeFusionText.getText();
		if( rangeFusion != null )
		{  
			String[] ranges = rangeFusion.split("[,;]");
			for( int i = 0; i < ranges.length; i++ )
			{
				String[] subset = ranges[i].split("-");
				int lowerBound = -1, bound = -1;
				for( int j = 0; j < subset.length; j++ )
				{
					try {
						bound = Integer.valueOf(subset[j].trim());
						if( lowerBound == -1 )
						{
							lowerBound = bound;
							includeRange.add(lowerBound);
						} 
					} catch(NumberFormatException e) {
						Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
					}
				}

				for( int k = lowerBound + 1; k <= bound; k++ )
				{
					includeRange.add(k);
				}
			}
		}

		if( includeRange.size() == 0 ) 
		{
			includeRange = null;
		}
		imageIndex = 0;

		return (includeRange != null);
	}

	/**
	 * Creates or updates the histogram / LUT panel and opacity panels when a new image is loaded.
	 */
	private void updateHistoLUTPanels()
	{
		if ( volOpacityPanel == null )
		{
//			volOpacityPanel = new JPanelVolumeOpacity(volumeImage.GetImage(), null, volumeImage.GetGradientMagnitudeImage(), null, true);
			volOpacityPanel = new JPanelVolumeOpacity(volumeImage.GetImage(), null, null, null, true);
			volOpacityPanel.addPropertyChangeListener(this);
			opacityPanel.removeAll();
			opacityPanel.add( volOpacityPanel.getMainPanel() );
		}
		else
		{
//			volOpacityPanel.setImages( volumeImage.GetImage(), null, volumeImage.GetGradientMagnitudeImage(), null, true );
			volOpacityPanel.setImages( volumeImage.GetImage(), null, null, null, true );
			opacityPanel.validate();
		}
		if ( lutHistogramPanel == null )
		{
			lutHistogramPanel = new JFrameHistogram(this, volumeImage.GetImage(), null, volumeImage.getLUT(), null);
			lutHistogramPanel.histogramLUT(true, false, true);
			lutPanel.removeAll();
			lutPanel.add(lutHistogramPanel.getContainingPanel());
		}
		else
		{
			lutHistogramPanel.setImages(volumeImage.GetImage(), null, volumeImage.getLUT(), null);
			lutHistogramPanel.histogramLUT(true, false, true);
			lutHistogramPanel.redrawFrames();
		}
		volumeImage.GetImage().addImageDisplayListener(this);
	}

	private void updateClipPanel()
	{
		if ( clipGUI == null ) 
		{
			clipGUI = new JPanelClip_WM(volumeRenderer);
		}
		else
		{
			clipGUI.setRenderer(volumeRenderer);
			clipPanel.removeAll();
		}
		clipPanel.add(clipGUI.getMainPanel() );
//        clipGUI.resizePanel(clipPanel.getWidth(), 400);
		clipPanel.revalidate();
	}

}
