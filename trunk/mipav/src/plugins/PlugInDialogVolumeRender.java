
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
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
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
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;
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
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class PlugInDialogVolumeRender extends JFrame implements ActionListener, AlgorithmInterface, PropertyChangeListener, ViewImageUpdateInterface {

	public static final int EditNONE = 0;
	public static final int EditSeamCells = 1;
	public static final int EditLattice = 2;
	public static final int EditAnnotations = 3;
	public static final int ReviewResults = 4;
	
	private JPanel algorithmsPanel;
	private VOIVector annotationList;
	private Vector<String> annotationNames;
	private VOIVector annotations;
	private JButton backButton;
	private String baseFileDir;
	private JTextField  baseFileLocText;
	private String baseFileName;
	private JTextField  baseFileNameText;
	private JProgressBar batchProgress;
	private JRadioButton buildLattice;
	private JPanel buttonPanel;
	private JRadioButton calcMaxProjection;
	private JPanel choicePanel;
	private JButton closeButton;
	private JRadioButton createAnimation;
	private JDialogStandalonePlugin dialogGUI;
	private JButton doneButton;
	private JRadioButton editAnnotations;
	private JRadioButton editLattice;
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

	private JPanel lutPanel;

	private JButton newLatticeButton; 

	private JButton nextButton;
	
	private JPanel opacityPanel;
	
	private PlugInDialogVolumeRender parent;
	
	private VOIVector[] potentialLattices;
	private JTextField rangeFusionText;
	
	private JRadioButton reviewResults;
	
	private JRadioButton segmentSeamCells;
	
	private JButton startButton;

	private JTabbedPane tabbedPane;


	private VolumeTriPlanarInterface triVolume;

	private VOILatticeManagerInterface voiManager;

	private JPanelVolumeOpacity volOpacityPanel;
	private VolumeImage volumeImage;
	private Container volumePanel;
	private VolumeTriPlanarRender volumeRenderer;
	private ModelImage wormImage;
	
	
	public PlugInDialogVolumeRender()
	{
		this.editMode = EditNONE;
		init(true);
		setVisible(true);
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
	/**
	 * Closes dialog box when the OK button is pressed and calls the algorithm.
	 *
	 * @param  event  Event that triggers function.
	 */
	public void actionPerformed(ActionEvent event)
	{
		String command = event.getActionCommand();
		Object source = event.getSource();
		if ( editMode == EditNONE )
		{
			setVariables();
			if (command.equals("start"))
			{
				startButton.setEnabled(false);
				if ( segmentSeamCells.isSelected() )
				{
					PlugInDialogWormLatticeStraighten.segmentSeamCells( batchProgress, includeRange, baseFileDir, baseFileNameText.getText() );
					segmentSeamCells.setSelected(false);
					editSeamCells.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( buildLattice.isSelected() )
				{
					PlugInDialogWormLatticeStraighten.buildLattice( batchProgress, includeRange, baseFileDir, baseFileNameText.getText());
					editLattice.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( latticeStraighten.isSelected() )
				{
					PlugInDialogWormLatticeStraighten.latticeStraighten( batchProgress, includeRange,  baseFileDir, baseFileNameText.getText() );
					reviewResults.setSelected(true);
					startButton.setEnabled(true);
				}
				else if ( calcMaxProjection.isSelected() )
				{
					PlugInDialogWormLatticeStraighten.createMaximumProjectionAVI( batchProgress, includeRange,  baseFileDir, baseFileNameText.getText() );
					startButton.setEnabled(true);
				}
				else if ( editSeamCells.isSelected() )
				{
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
				else if ( editAnnotations.isSelected() )
				{
					if ( !integratedDisplay )
					{
						PlugInDialogVolumeRender editor = new PlugInDialogVolumeRender( this, PlugInDialogVolumeRender.EditAnnotations, includeRange, imageIndex, baseFileDir, baseFileNameText.getText() );
						editor.setLocation( this.getWidth(), this.getY() );
						editor.openAnnotations();
					}
					else
					{
						editMode = EditAnnotations;
						openAnnotations();
					}
				}
				else if ( reviewResults.isSelected() )
				{
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
				else if ( createAnimation.isSelected() )
				{
					this.setVisible(false);
					annotationAnimationFromSpreadSheet();
				}
			}

			baseFileNameText.setEnabled( !createAnimation.isSelected() );
			rangeFusionText.setEnabled( !createAnimation.isSelected() );
		}
		else
		{
			if ( command.equals("next") )
			{
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
				else if ( editMode == EditAnnotations )
				{
					openAnnotations();
				}
				else if ( editMode == ReviewResults )
				{
					openStraightened();
				}
			}
			else if ( command.equals("back") )
			{
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
				else if ( editMode == EditAnnotations )
				{
					openAnnotations();
				}
				else if ( editMode == ReviewResults )
				{
					openStraightened();
				}
			}
			else if (command.equals("done"))
			{			
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
			}
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
				this.validate();
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
			dispose();
		}
	}
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

//		System.err.println( "algorithmPerformed" );
		voiManager = new VOILatticeManagerInterface( null, volumeImage.GetImage(), null, 0, true, null );
		volumeRenderer.setVOILatticeManager(voiManager);
		if ( (editMode == EditSeamCells) || (editMode == EditAnnotations) )
		{
			if ( annotations != null )
			{
				if ( annotations.size() > 0 )
				{
					voiManager.setAnnotations(annotations);

					for (int i = 0; i < annotations.elementAt(0).getCurves().size(); i++)
					{
						final VOIText text = (VOIText) annotations.elementAt(0).getCurves().elementAt(i);
						text.createVolumeVOI( volumeImage, volumeRenderer.getTranslate() );    			
					}
				}
			}
			voiManager.editAnnotations(editMode == EditSeamCells);
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
		if ( wormImage != null )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}
		if ( mode == EditSeamCells )
		{
			buildLattice.setSelected(true);
		}
		else if ( mode == EditLattice )
		{
			editAnnotations.setSelected(true);
		}
		else if ( mode == EditAnnotations )
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

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		String propertyName = event.getPropertyName();
		if ( propertyName.equals("Opacity") )
		{
			final TransferFunction kTransfer = volOpacityPanel.getCompA().getOpacityTransferFunction();
			volumeImage.UpdateImages(kTransfer, 0, null);
		}
	}

	@Override
	public void setSlice(int slice) {
		// TODO Auto-generated method stub
		
	}
	
	@Override
	public void setTimeSlice(int tSlice) {
		// TODO Auto-generated method stub
		
	}
	@Override
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}
	@Override
	public boolean updateImages() {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(volumeImage.GetLUT());
		}
		return false;
	}
	@Override
	public boolean updateImages(boolean flag) {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(volumeImage.GetLUT());
		}
		return false;
	}
	@Override
	public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
		if ( volumeImage != null )
		{
			volumeImage.UpdateImages(LUTa);
		}
		return false;
	}

	protected void openAnnotations()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( openImages( voiFile, fileName ) )
				{
					if ( annotations != null )
					{
						annotations.clear();
						annotations = null;
					}
					annotations = new VOIVector();
					fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editAnnotationOutput;
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);

					if ( annotations.size() == 0 )
					{
						fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editAnnotationInput;
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);
					}
					if ( voiManager != null )
					{
						voiManager.setAnnotations(annotations);
						voiManager.editAnnotations(false);
					}
				}
			}
		}
	}

	protected boolean openImages( File imageFile, String fileName )
	{
		if ( imageFile.exists() )
		{
			//					System.err.println( fileName );
			gpuPanel.setBorder(JDialogBase.buildTitledBorder(fileName));
			FileIO fileIO = new FileIO();
			if ( wormImage != null ) {
				wormImage.disposeLocal();
				wormImage = null;
			}
			wormImage = fileIO.readImage(fileName, imageFile.getParent() + File.separator, false, null); 
			wormImage.calcMinMax();      
			
			if ( voiManager != null )
			{
				voiManager.setImage(wormImage);
			}
			if ( volumeImage == null )
			{
				volumeImage = new VolumeImage(false, wormImage, "", null, 0);
				updateHistoLUTPanels();
			}
			else
			{
				volumeImage.UpdateData(wormImage);
				updateHistoLUTPanels();
			}
			if ( volumeRenderer == null )
			{
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
				if ( !volumeRenderer.isVisible() )
				{
					volumeRenderer.setVisible(true);
				}
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

	protected void openLattice()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( openImages( voiFile, fileName ) )
				{
					if ( potentialLattices != null )
					{
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							potentialLattices[i].clear();
						}
					}
					if ( potentialLattices == null )
					{
						potentialLattices = new VOIVector[5];
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							potentialLattices[i] = new VOIVector();
						}
					}

					latticeSelectionPanel.removeAll();
					latticeSelectionPanel.setVisible(false);
					fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editLatticeOutput;
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, potentialLattices[0], false);

					int latticeIndex = -1;
					if ( potentialLattices[0].size() == 0 )
					{
						for ( int i = 0; i < potentialLattices.length; i++ )
						{
							fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.autoLatticeGenerationOutput + i;
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, potentialLattices[i], false);

							if ( potentialLattices[i].size() != 0 )
							{
								latticeSelectionPanel.add(latticeChoices[i]);
								if ( latticeIndex == -1 )
								{
									latticeIndex = i;
								}
							}
						}
						latticeSelectionPanel.add(newLatticeButton);
						latticeSelectionPanel.setVisible(true);
					}
					else
					{
						latticeIndex = 0;
					}
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

	protected void openSeamCells()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( openImages( voiFile, fileName ) )
				{
					if ( annotations != null )
					{
						annotations.clear();
						annotations = null;
					}
					annotations = new VOIVector();

					fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editSeamCellOutput;
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);

					if ( annotations.size() == 0 )
					{
						fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.autoSeamCellSegmentationOutput;
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, annotations, true);
					}

					if ( voiManager != null )
					{
						voiManager.setAnnotations(annotations);
						voiManager.editAnnotations(true);
					}
				}
			}
		}
	}

	protected void openStraightened()
	{
		if ( includeRange != null )
		{			
			if ( (imageIndex >= 0) && (imageIndex < includeRange.size()) )
			{
				String imageName = baseFileName + "_" + includeRange.elementAt(imageIndex) + "_straight.tif";
				String subDirName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator;
				File voiFile = new File(baseFileDir + File.separator + subDirName + PlugInDialogWormLatticeStraighten.outputImages + File.separator + imageName);
				if ( openImages( voiFile, imageName ) )
				{
					VOIVector results = new VOIVector();
					String voiDir = new String( baseFileDir + File.separator + subDirName + PlugInDialogWormLatticeStraighten.straightenedAnnotations + File.separator );
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, results, true);
					
					voiDir = new String( baseFileDir + File.separator + subDirName + PlugInDialogWormLatticeStraighten.straightenedLattice + File.separator );
					PlugInDialogWormLatticeStraighten.loadAllVOIsFrom(wormImage, voiDir, true, results, true);
				}
			}
		}
	}

	private void annotationAnimationFromSpreadSheet()
	{
		int[] extents = new int[3];
		VOIVector tempList = new VOIVector();
		Vector< int[] > timesList = new Vector< int[] >();
		ViewJProgressBar progress = new  ViewJProgressBar( "Generating Animation", "", 0, 100, false);
		progress.dispose();
		progress = null;

		String inputDirName = baseFileDir + File.separator;
		//		System.err.println( inputDirName );
		final File inputFileDir = new File(inputDirName);

		Vector3f min = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
		Vector3f max = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
		int timeCount, maxTimeCount = -1, minTimeCount = Integer.MAX_VALUE;
		int maxIndex = -1;
		int fileIndex = 0;
		if (inputFileDir.exists() && inputFileDir.isDirectory()) {
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

				int startTime = -1;
				int endTime = -1;
				timeCount = 0;
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
							if ( startTime == -1 )
							{
								startTime = time;
							}
							endTime = time;
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
							timeCount++;

							min.min( text.elementAt(0) );
							max.max( text.elementAt(0) );

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
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if ( timeCount < minTimeCount )
				{
					minTimeCount = timeCount;
					maxIndex = fileIndex;
				}
				if ( timeCount > maxTimeCount )
				{
					maxTimeCount = timeCount;
					maxIndex = fileIndex;
				}
				fileIndex++;
			}
		}

		//        System.err.println( minTimeCount + " " + maxTimeCount + " " + (minTimeCount == maxTimeCount ) );

		//        System.err.println( timesList.size() + " " + tempList.size() );
		int[] times = timesList.elementAt( maxIndex );
		VOI curve = tempList.elementAt( maxIndex );
		//		for ( int j = 0; j < times.length; j++ )
		//		{
		//			System.err.println( curve.getName() + " " + times[j] );
		//		}


		annotationList = new VOIVector();
		for ( int i = 0; i < times.length; i++ )
		{
			int timeStep = times[i];
			VOI annotation = new VOI( (short)i, "time" + timeStep, VOI.ANNOTATION, 0 );

			//			System.err.print( timeStep );
			for ( int j = 0; j < timesList.size(); j++ )
			{
				int[] currentTimes = timesList.elementAt(j);
				for ( int k = 0; k < currentTimes.length; k++ )
				{
					if ( timeStep == currentTimes[k] )
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

		animationImage.setImageDirectory( outputDirName );		
		triVolume = new VolumeTriPlanarInterface(animationImage, null);
		triVolume.addConfiguredListener(this);
	}

	private void init()
	{
		MipavInitGPU.InitGPU();

		setResizable(true);
		setForeground(Color.black);
		setTitle("Untwisting C.elegans - lattice - 1.0");
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

		baseFileLocText = gui.buildFileField("Data directory: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());
		//		panel.add(buttonPanel, gbc);

		ButtonGroup group = new ButtonGroup();
		algorithmsPanel = makeAlgorithmsPanel(gui, group);
		editPanel = makeEditPanel(gui, group);

		choicePanel = new JPanel( new GridLayout(1,2) );
		choicePanel.add(algorithmsPanel);
		choicePanel.add(editPanel);

		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(choicePanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);


		setLocation(0, 0);
		pack();
		setResizable(true);

		segmentSeamCells.setSelected(true);
	}


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
		setTitle("Untwisting C.elegans - lattice - 1.0");
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

		baseFileLocText = gui.buildFileField("Data directory: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
		inputsPanel.add(baseFileLocText.getParent(), gbc);
		gbc.gridy++;

		baseFileNameText = gui.buildField("Base images name: ", "Decon");
		inputsPanel.add(baseFileNameText.getParent(), gbc);
		gbc.gridy++;

		rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
		inputsPanel.add(rangeFusionText.getParent(), gbc);
		gbc.gridy++;

		buttonPanel = new JPanel();
		startButton = gui.buildButton("start");
		startButton.addActionListener(this);
		buttonPanel.add( startButton );
		closeButton = gui.buildButton("close");
		closeButton.addActionListener(this);
		buttonPanel.add( closeButton );
		buttonPanel.add(new JPanel());

		ButtonGroup group = new ButtonGroup();
		algorithmsPanel = makeAlgorithmsPanel(gui, group);
		editPanel = makeEditPanel(gui, group);

		choicePanel = new JPanel( new GridLayout(1,2) );
		choicePanel.add(algorithmsPanel);
		choicePanel.add(editPanel);

		dialogGUI.getContentPane().add(inputsPanel, BorderLayout.NORTH);
		dialogGUI.getContentPane().add(choicePanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

//		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);

		lutPanel = new JPanel();
		opacityPanel = new JPanel();
		tabbedPane = new JTabbedPane();
		tabbedPane.addTab("LUT", null, lutPanel);
		tabbedPane.addTab("Opacity", null, opacityPanel);
		tabbedPane.setVisible(false);
		
		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.add( dialogGUI.getContentPane(), BorderLayout.NORTH );
		leftPanel.add( tabbedPane, BorderLayout.SOUTH );
		
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
	private Container initGPUPanel( int editMode )
	{
		MipavInitGPU.InitGPU();

		dialogGUI = new JDialogStandalonePlugin();
		GuiBuilder gui = new GuiBuilder(dialogGUI);

		buttonPanel = new JPanel();
		backButton = gui.buildButton("back");
		backButton.addActionListener(this);
		backButton.setActionCommand("back");
		backButton.setVisible(true);
		backButton.setEnabled(false);
		buttonPanel.add( backButton );

		nextButton = gui.buildButton("next");
		nextButton.addActionListener(this);
		nextButton.setActionCommand("next");
		nextButton.setVisible(true);
		nextButton.setEnabled(true);
		buttonPanel.add( nextButton );

		doneButton = gui.buildButton("done");
		doneButton.addActionListener(this);
		doneButton.setActionCommand("done");
		doneButton.setVisible(true);
		doneButton.setEnabled(true);
		buttonPanel.add( doneButton );

		ButtonGroup latticeGroup = new ButtonGroup();
		latticeSelectionPanel = new JPanel();
		latticeChoices = new JRadioButton[5];
		for ( int i = 0; i < latticeChoices.length; i++ )
		{
			latticeChoices[i] = gui.buildRadioButton("lattice_" + i, i==0 );
			latticeChoices[i].addActionListener(this);
			latticeGroup.add(latticeChoices[i]);
		}
		newLatticeButton = gui.buildButton("new lattice");
		newLatticeButton.addActionListener(this);
		newLatticeButton.setActionCommand("newLattice");
		newLatticeButton.setVisible(true);
		newLatticeButton.setEnabled(true);

		latticeSelectionPanel.add(newLatticeButton);
		buttonPanel.add( latticeSelectionPanel );
		latticeSelectionPanel.setVisible(false);


		gpuPanel = new JPanel(new BorderLayout());
		final int imagePanelWidth = (int) (Toolkit.getDefaultToolkit().getScreenSize().width * 0.5f);
		final int imagePanelHeight = (int) (Toolkit.getDefaultToolkit().getScreenSize().height * 0.5f);

		gpuPanel.setPreferredSize(new Dimension(imagePanelWidth, imagePanelHeight));
		gpuPanel.setMinimumSize(new Dimension(250, 250));
		gpuPanel.setBorder(JDialogBase.buildTitledBorder("Volume Display"));


		dialogGUI.getContentPane().add(gpuPanel, BorderLayout.CENTER);
		dialogGUI.getContentPane().add(buttonPanel, BorderLayout.SOUTH);

//		getContentPane().add(dialogGUI.getContentPane(), BorderLayout.CENTER);
//
//		pack();
//		setResizable(true);
		return dialogGUI.getContentPane();
	}
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
		batchProgress = new JProgressBar(0,100);
		panel.add(batchProgress, gbc);

		group.add(segmentSeamCells);
		group.add(buildLattice);
		group.add(latticeStraighten);
		group.add(calcMaxProjection);
		
		return panel;
	}

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
		editLattice = gui.buildRadioButton("4). edit lattice", false );
		editLattice.addActionListener(this);
		editLattice.setActionCommand("editLattice");
		panel.add(editLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		editAnnotations = gui.buildRadioButton("5). add annotations", false );
		editAnnotations.addActionListener(this);
		editAnnotations.setActionCommand("editAnnotations");
		panel.add(editAnnotations.getParent(), gbc);
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
		group.add(editAnnotations);
		group.add(createAnimation);
		group.add(reviewResults);

		return panel;
	}

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
		else if ( editMode == EditAnnotations )
		{
			saveAnnotations();
		}
	}

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

		String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editAnnotationOutput;  
		String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
		WormSegmentation.saveAllVOIsTo(voiDir, wormImage);
	}

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
		voiManager.saveLattice( baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(imageIndex) + File.separator, 
				PlugInDialogWormLatticeStraighten.editLatticeOutput );
	}

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

		String fileName = baseFileName + "_" + includeRange.elementAt(imageIndex) + File.separator + PlugInDialogWormLatticeStraighten.editSeamCellOutput;  
		String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
		WormSegmentation.saveAllVOIsTo(voiDir, wormImage);
	}

	private boolean setVariables()
	{	    
		baseFileName = baseFileNameText.getText();
		baseFileDir = baseFileLocText.getText();
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

	private void updateHistoLUTPanels()
	{
		if ( volOpacityPanel == null )
		{
			volOpacityPanel = new JPanelVolumeOpacity(volumeImage.GetImage(), null, volumeImage.GetGradientMagnitudeImage(), null, true);
			volOpacityPanel.addPropertyChangeListener(this);
			opacityPanel.add( volOpacityPanel.getMainPanel() );
		}
		else
		{
			volOpacityPanel.setImages( volumeImage.GetImage(), null, volumeImage.GetGradientMagnitudeImage(), null, true );
			opacityPanel.validate();
		}
		if ( lutHistogramPanel == null )
		{
			lutHistogramPanel = new JFrameHistogram(this, volumeImage.GetImage(), null, volumeImage.getLUT(), null);
			lutHistogramPanel.histogramLUT(true, false, true);
			lutPanel.add(lutHistogramPanel.getContainingPanel());
		}
		else
		{
			lutHistogramPanel.setImages(volumeImage.GetImage(), null, volumeImage.getLUT(), null);
			lutHistogramPanel.histogramLUT(true, false, true);
		}
		volumeImage.GetImage().addImageDisplayListener(this);
	}


}
