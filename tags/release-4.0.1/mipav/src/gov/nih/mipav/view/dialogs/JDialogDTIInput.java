package gov.nih.mipav.view.dialogs;

import java.awt.event.ActionEvent;
import gov.nih.mipav.view.dialogs.*;
import java.awt.*;
import javax.swing.*;
import java.io.*;
import java.util.Vector;

import javax.media.j3d.*;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;

import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJComponentDTIImage;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.renderer.volumeview.GPUVolumeRender;
import gov.nih.mipav.view.renderer.surfaceview.JPanelSurface;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.NumericalAnalysis.*;
import java.io.FileInputStream;
import gov.nih.mipav.model.structures.jama.*;

/** Dialog for specifying Diffusion Tensor Images.
 *
 *  Diffusion Tensor images may be loaded in one of the following
 * ways, specified by the dialog types: DWI, DTI, EG_FA, or TRACTS.
 *
 * DWI: The DWI dialog loads the raw Diffusion Weighted
 * Images. Calculates the tensor image from the set of weighed
 * images. The dilog requires the user to specify the raw image
 * dimensions, and the number of weighted image sets. The user must
 * also specify the mean noise value, the name of the B-Matrix file,
 * and the .path file which specifies the locations of the weighted
 * images in the directory structure. Once the tensor image is
 * calculated, the eigen-vector and functional anisotropy images are
 * calculated and passed to the DialogDTIColorDisplay dialog.  The
 * user also has the option of creating the fiber-tract bundles from
 * the tensor image.
 *
 * DTI: The DTI dialog loads a previously-calculated Diffusion Tensor
 * image and calculates the eigen-vector image and the functional
 * anisotropy image. The DialogDTIColorDisplay dialog is then
 * launched. The user specifies the tensor image and has the option of
 * creating the fiber-tract bundles from the tensor image.
 *
 * EG_FA: The EG_FA dialog loads previously calculated eigen-vector
 * and functional anisotropy images and launches the
 * DialogDTIColorDisplay dialog
 *
 * TRACTS: The tracts dialog is launched from the ViewJFrameViewWM
 * class. It enables the user to add the fiber-tract visualization to
 * the GPU Volume display. The user can limit the number of tracts,
 * the minimum and maximum tract lengths, or specify that tracts that
 * pass through a VOI are loaded.
 */
public class JDialogDTIInput extends JDialogBase
    implements ViewImageUpdateInterface
{

    /** Types of dialogs: */
    /** Diffusion Weighted Images dialog: */
    public static final int DWI = 0;
    /** Diffusion Tensor Image dialog: */
    public static final int DTI = 1;
    /** EigenVector and Functional Anisotropy dialog: */
    public static final int EG_FA = 2;
    /** Fiber Bundle tracts dialog: */
    public static final int TRACTS = 3;

    /** Eigenvector image **/
    private ModelImage m_kEigenVectorImage;
    
    /** Anisotropy image **/
    private ModelImage m_kAnisotropyImage;

    /** Diffusion Tensor image. */
    private ModelImage m_kDTIImage = null;

    /** LUT of input image **/
    private ModelLUT m_kLUTa;

    /** EigenVector file input path name text box. */
    private JTextField m_kEigenVectorPath;
    /** Anisotropy file input path name text box. */
    private JTextField m_kAnisotropyPath;
    /** Diffusion Tensor file input path name text box. */
    private JTextField m_kDTIPath;
    /** BMatrix file input path name text box. */
    private JTextField m_kBMatrixPath;
    /** Diffusion Weighted Images .path file input path name text box. */
    private JTextField m_kDWIPath;
    /** Fiber bundle tract file input path name text box. */
    private JTextField m_kTractPath;

    /** Diffusion Weighted Image image dimension 1, input text box. */
    private JTextField m_kTextDim1;

    /** Diffusion Weighted Image image dimension 2, input text box. */
    private JTextField m_kTextDim2;

    /** Diffusion Weighted Image image dimension 3, input text box. */
    private JTextField m_kTextDim3;

    /** Diffusion Weighted Image image dimension 4, input text box. */
    private JTextField m_kTextDim4;

    /** Diffusion Weighted Image image resolutions 1, input text box. */
    private JTextField m_kTextRes1;

    /** Diffusion Weighted Image image resolutions 2, input text box. */
    private JTextField m_kTextRes2;

    /** Diffusion Weighted Image image resolutions 3, input text box. */
    private JTextField m_kTextRes3;

    /** Diffusion Weighted Image image resolutions 4, input text box. */
    private JTextField m_kTextRes4;

    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;
    /** List of file names for the Diffusion Weighted Images, from the .path file. */
    private String[][] m_aakDWIList = null;

    /** Number of slices in the Diffusion Weighted Images series. */
    private int m_iSlices = 0;

    /** Number of weights in the Diffusion Weighted Images series. */
    private int m_iWeights = 0;

    /** X-dimensions for Diffusion Weighted Images. */
    private int m_iXDim = 0;
    /** Y-dimensions for Diffusion Weighted Images. */
    private int m_iYDim = 0;

    /** Dialog type. */
    private int m_iType;

    /** GPUVolumeRender object for loading fiber bundle tracts. */
    private GPUVolumeRender m_kVolumeDisplay = null;

    /** When outputing the fiber bundle tracts. */
    private boolean m_bFirstWrite = true;

    /** Checkbox for tract reconstruction. */
    private JCheckBox m_kReconstructTracts;
    /** Fiber bundle tract output file text box. */
    private JTextField m_kTractOutputPath;

    /** For TRACTS dialog: number of tracts to display. */
    private JTextField m_kTractsLimit;
    /** For TRACTS dialog: minimum tract length to display. */
    private JTextField m_kTractsMin;
    /** For TRACTS dialog: maximum tract length to display. */
    private JTextField m_kTractsMax;
    /** Tract input file. */
    private File m_kTractFile = null;
    /** When selected, only tracts that intersect the VOI are displayed. */
    private JCheckBox m_kUseVOICheck = null;

    /** Number of different BMatrix rows: */
    private int m_iBOrig = 0;
    /** */
    private int[] repidx;

    /** Mask of the image that shows only the brain: */
    private ModelImage m_kBrainImage = null;
    /** ViewJFrameImage for displaying the Mask of the image that shows only the brain: */
    private ViewJFrameImage m_kBrainFrame = null;

    /** Create a new JDialogDTIInput of one of the four types:
     * @param iType, type of Diffusion Tensor Input dialog to create.
     */
    public JDialogDTIInput( int iType )
    {
        super();
        init( iType );
        m_iType = iType;
    }

    /** Create a new JDialogDTIInput of one of the four types:
     * @param iType, type of Diffusion Tensor Input dialog to create.
     * @param kDisplay, reference to the GPUVolumeRender display for
     * loading fiber bundle tracts.
     */
    public JDialogDTIInput( int iType, GPUVolumeRender kDisplay ) 
    {
        super();
        init( iType );
        m_iType = iType;
        m_kVolumeDisplay = kDisplay;
    }

    /** The JDialogDTIInput interface. 
     * @param iType, type of Diffusion Tensor Input dialog to create.
     */
    private void init( int iType )
    {
        setForeground(Color.black);
        setTitle("Select Diffusion Tensor Input");

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        GridBagConstraints gbcMain = new GridBagConstraints();
        gbcMain.gridx = 0;
        gbcMain.gridy = 0;
        JPanel kMainPanel = new JPanel( new GridBagLayout() );
        if ( iType == DWI )
        {
            kMainPanel.add( createDWIPanel(), gbcMain );
        }
        else if ( iType == DTI )
        {
            kMainPanel.add( createDTIPanel(), gbcMain );
        }
        else if ( iType == EG_FA )
        {
            kMainPanel.add( createEigenPanel(), gbcMain );
        }
        else if ( iType == TRACTS )
        {
            kMainPanel.add( createTractPanel(), gbcMain );
        }
        gbcMain.gridy++;
        kMainPanel.add( buttonPanel, gbcMain );

        getContentPane().add(kMainPanel);
        pack();
        setVisible(true);
    }

    /** ActionListener event.
     * @param kAction, ActionEvent
     */
    public void actionPerformed(ActionEvent kAction)
    {
        Object kSource = kAction.getSource();
        String kCommand = kAction.getActionCommand();

        if ( kSource == cancelButton )
        {
            dispose();
        }
	
        if ( kCommand.equalsIgnoreCase("eigenvectorBrowse") )
	{
	    loadEigenVectorFile();
	}
        else if ( kCommand.equalsIgnoreCase("anisotropyBrowse") )
	{
            loadAnisotropyFile();
        }
        else if ( kCommand.equalsIgnoreCase("DTIBrowse") )
	{
            loadDTIFile();
        }
        else if ( kCommand.equalsIgnoreCase("DWIBrowse") )
	{
            loadDWIFile();
        }
        else if ( kCommand.equalsIgnoreCase("BMatrixBrowse") )
	{
            loadBMatrixFile();
        }
        else if ( kCommand.equalsIgnoreCase("tractBrowse") )
	{
            loadTractFile();
        }
        else if ( kCommand.equalsIgnoreCase("ok") )
	{
            if ( m_iType == DWI )
            {
                if ( m_kBMatrix == null || m_aakDWIList == null )
                {
                    MipavUtil.displayError("Both BMatrix and .path files are needed.");
                    return;
                }
                processDWI();
            }
            else if ( m_iType == DTI )
            {
                if( m_kDTIImage == null ) {
                    MipavUtil.displayError("Diffusion Tensor Image needed");
                    return;
                }
                processDTI();
            }
            else if ( m_iType == EG_FA )
            {
                if(m_kEigenVectorImage == null || m_kAnisotropyImage == null) {
                    MipavUtil.displayError("Both eigenvector and anisotropy files are needed");
                    return;
                }
                new DialogDTIColorDisplay(m_kEigenVectorImage, m_kAnisotropyImage, m_kLUTa, false);
                dispose();
            }
            else if ( m_iType == TRACTS )
            {
                processTractFile();
                dispose();
            }
        }
    }

    /**
     * Launches the JFileChooser for the user to select the BMatrix file. Loads the BMatrix file.
     */
    public void loadBMatrixFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose BMatrix file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            File kFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if ( !kFile.exists() || !kFile.canRead() )
            {
                return;
            }
            int iLength = (int)kFile.length();
            if ( iLength <= 0 )
            {
                return;
            }
            m_kBMatrixPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            try {
                BufferedReader in = new BufferedReader(new FileReader(kFile));
                String str;

                m_iWeights = Integer.valueOf(m_kTextDim4.getText()).intValue();
                m_kBMatrix = new GMatrixf( m_iWeights, 6 + 1 );

                String[] kBMatrixString = new String[m_iWeights];
                int nb = 0;

                repidx = new int[m_iWeights];
                for ( int iRow = 0; iRow < m_iWeights; iRow++ )
                {
                    str = in.readLine();

                    boolean gotit = false;
                    for (int j=0; j < nb ; j++) { 
                        if (str.equals(kBMatrixString[j]))
                        {
                            gotit=true;
                            repidx[iRow]=j;
                            break;
                        }
                    }
                    if (!gotit)
                    { 
                        kBMatrixString[nb]=str;
                        repidx[iRow]=nb;
                        nb=nb+1;
                    }	

                   java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                    for ( int iCol = 0; iCol < 6; iCol++ )
                    {
                        float fValue = Float.valueOf(st.nextToken()).floatValue();
                        m_kBMatrix.Set( iRow, iCol, fValue );
                    }
                    m_kBMatrix.Set( iRow, 6, 1f );
                } 
                System.out.println("number of different b: "+nb);
                in.close();

                m_iBOrig = nb;

            } catch (IOException e) {}
        }
    }

    
    /** Creates the weighted mask image for the tensor calculation from the diffusion weighted images.
     * @param kMaskImage, the image masking the diffusion weighted images so only the brain portions are used.
     * @return float[][][] containing the weights used in the tensor calculation.
     */
    private float[][][] createTensorWeights(ModelImage kMaskImage) {
        m_iSlices = Integer.valueOf(m_kTextDim3.getText()).intValue();
        m_iXDim = Integer.valueOf(m_kTextDim1.getText()).intValue();
        m_iYDim = Integer.valueOf(m_kTextDim2.getText()).intValue();

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "creating weights mask...", 0, m_iBOrig, true);

        float[][][] norm = new float[m_iBOrig][m_iSlices][m_iXDim*m_iYDim];
        float[][][] wmask = new float[m_iWeights][m_iSlices][m_iXDim*m_iYDim];

        for (int i=0; i<m_iBOrig; i++)
        {
            for (int j=0; j< m_iWeights; j++)
            {
                if ( repidx[j] == i )
                {
                    for (int slice = 0; slice < m_iSlices; slice++)
                    { 
                        File kFile = new File( m_aakDWIList[slice][j] );
                        if ( !kFile.exists() || !kFile.canRead() )
                        {
                            MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                            return null;
                        }
                        int iLength = (int)kFile.length();
                        if ( iLength <= 0 )
                        {
                            MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                            return null;
                        }
                        byte[] abSliceData = new byte[iLength];
                        try {
                            abSliceData = new byte[iLength];
                            FileInputStream kFileReader = new FileInputStream(kFile);
                            kFileReader.read( abSliceData,0,iLength);
                            kFileReader.close();
                        } catch (IOException e ) {}


                        for ( int iY = 0; iY < m_iYDim; iY++ )
                        {
                            for ( int iX = 0; iX < m_iXDim; iX++ )
                            {
                                int iIndex = (iY * m_iXDim) + iX;
                                if ( kMaskImage.getBoolean( slice*m_iYDim*m_iXDim + iIndex) )
                                {
                                    float fValue = readFloat( abSliceData, iIndex );
                                    norm[i][slice][iIndex] += fValue;
                                }
                                else
                                {
                                    norm[i][slice][iIndex] = 0;
                                }
                            }
                        }
                    }
                }
            }
            kProgressBar.updateValue(i+1);
        }		

        kProgressBar.getProgressBar().setMaximum(m_iWeights);
        for (int j=0; j< m_iWeights; j++)
        {
            for (int slice = 0; slice < m_iSlices; slice++)
            { 
                File kFile = new File( m_aakDWIList[slice][j] );
                if ( !kFile.exists() || !kFile.canRead() )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                    return null;
                }
                int iLength = (int)kFile.length();
                if ( iLength <= 0 )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[slice][j] + ".");
                    return null;
                }
                byte[] abSliceData = new byte[iLength];
                try {
                    abSliceData = new byte[iLength];
                    FileInputStream kFileReader = new FileInputStream(kFile);
                    kFileReader.read( abSliceData,0,iLength);
                    kFileReader.close();
                } catch (IOException e ) {}


                for ( int iY = 0; iY < m_iYDim; iY++ )
                {
                    for ( int iX = 0; iX < m_iXDim; iX++ )
                    {
                        int iIndex = (iY * m_iXDim) + iX;

                        if ( norm[repidx[j]][slice][iIndex] == 0 )
                        {
                            wmask[j][slice][iIndex]= 0;
                        }
                        else
                        {
                            if ( kMaskImage.getBoolean( slice*m_iYDim*m_iXDim + iIndex) )
                            {
                                float fValue = readFloat( abSliceData, iIndex );
                                wmask[j][slice][iIndex]= fValue/norm[repidx[j]][slice][iIndex];
                            }
                        }
                    }
                }
            }
            kProgressBar.updateValue(j+1);
        }

        kProgressBar.dispose();
        return wmask;	
    }



    /**
     * Launches the JFileChooser for the user to select the Diffusion Weighted Images .path file. Loads the .path file.
     */
    public void loadDWIFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Tensor .path file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            File kFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if ( !kFile.exists() || !kFile.canRead() )
            {
                return;
            }
            int iLength = (int)kFile.length();
            if ( iLength <= 0 )
            {
                return;
            }
            m_kDWIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            m_kTractOutputPath.setText(chooser.getSelectedFile().getAbsolutePath() + "_tract" );

            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            m_iSlices = Integer.valueOf(m_kTextDim3.getText()).intValue();
            m_iWeights = Integer.valueOf(m_kTextDim4.getText()).intValue();
            m_aakDWIList = new String[m_iSlices][m_iWeights];
            try {
                BufferedReader in = new BufferedReader(new FileReader(kFile));
                String str;
                int iSlices = 0;
                int iWeight = 0;
                for ( int i = 0; i < m_iSlices; i++ )
                {
                    for ( int j = 0; j < m_iWeights; j++ )
                    {
                        str = in.readLine();
                        m_aakDWIList[i][j] = new String(chooser.getSelectedFile().getParentFile() + File.separator + str);
                    }
                }
                in.close();
            } catch (IOException e) {}
        }
    }

    /** Processes the Diffusion Tensor Image. Creates the eigen vector
     * and functional anisotropy images. Launched the
     * DialogDTIColorDisplay. */
    private void processDTI()
    {
        if ( m_kDTIImage == null )
        {
            MipavUtil.displayError("DTI file must be set to create eigen vector data.");
            return;
        }
        setCursor(new Cursor(Cursor.WAIT_CURSOR));
        calcEigenVector(m_kDTIImage);
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
	new DialogDTIColorDisplay(m_kEigenVectorImage, m_kAnisotropyImage, m_kLUTa, false);
	dispose();
    }

    /** First step in processing the diffusion weighted images. During
     * this step one of the weighted series is used to create a new
     * ModelImage, the ModelImage is processed with the
     * JDialogBrainSurfaceExtractor to extract the brain portions of
     * the image into a new image. */
    private void processDWI()
    {
        if ( m_kBMatrix == null )
        {
            MipavUtil.displayError("BMatrix file must be set to create tensor data.");
            return;
        }
        if ( m_aakDWIList == null )
        {
            MipavUtil.displayError("Path file must be set to create tensor data.");
            return;
        }
        
        setCursor(new Cursor(Cursor.WAIT_CURSOR));

        m_iSlices = Integer.valueOf(m_kTextDim3.getText()).intValue();
        m_iXDim = Integer.valueOf(m_kTextDim1.getText()).intValue();
        m_iYDim = Integer.valueOf(m_kTextDim2.getText()).intValue();

        int[] imageExtents = new int[]{m_iXDim, m_iYDim, m_iSlices};
        m_kBrainImage = new ModelImage( ModelStorageBase.FLOAT, imageExtents, new String( "BrainImage" ) );
        for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
        {
            int iWeight = 0;
            File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
            if ( !kFile.exists() || !kFile.canRead() )
            {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return;
            }
            int iLength = (int)kFile.length();
            if ( iLength <= 0 )
            {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return;
            }
            byte[] abSliceData = new byte[iLength];
            try {
                FileInputStream kFileReader = new FileInputStream(kFile);
                kFileReader.read(abSliceData,0,iLength);
                kFileReader.close();
            } catch (IOException e ) {}
            
            for ( int iY = 0; iY < m_iYDim; iY++ )
            {
                for ( int iX = 0; iX < m_iXDim; iX++ )
                {
                    int iIndex = (iY * m_iXDim) + iX;
                    float p = readFloat( abSliceData, iIndex );
                    
                    m_kBrainImage.set(iSlice*(m_iXDim*m_iYDim) + iIndex, p );
                }
            }
        }

        m_kBrainImage.addImageDisplayListener(this);
        m_kBrainFrame = new ViewJFrameImage(m_kBrainImage, null, new Dimension(610, 200), false);
        JDialogBrainSurfaceExtractor kExtractBrain = new JDialogBrainSurfaceExtractor( m_kBrainFrame, m_kBrainImage );
        kExtractBrain.setFilterGaussianStdDev(5);
        kExtractBrain.setFillHoles(false);
        kExtractBrain.setExtractPaint(true);
        kExtractBrain.callAlgorithm();
    }

    /** Second step in processing the diffusion weighted images. This
     * function is called once the brain extractor is complete.  The
     * brain image is transformed into a mask image, which is passed
     * to this function. The mask image limits where the tensor
     * calculations are performed.  The tensor is calculated, then the
     * eigen vectors and functional anisotropy images. The
     * DialogDTIColorDisplay is then launched.
     * @param kMaskImage, mask image representing the brain.
     */
    private void processDWI( ModelImage kMaskImage )
    {

        float[][][] aaafWeights = createTensorWeights( kMaskImage );
        
        int iLen = m_iXDim * m_iYDim * m_iSlices;
        float[] afTensorData = new float[iLen * 6];


        int vol = m_iWeights;
        int a0 = 1;
        
        double[][] bmatrix = new double[vol][6+a0];

        Matrix B = new Matrix( m_kBMatrix.GetRows(), 6+1 );
        for ( int iR = 0; iR < m_kBMatrix.GetRows(); iR++ )
        {
            for ( int iC = 0; iC < 6+1; iC++ )
            {
                B.set(iR, iC, m_kBMatrix.Get(iR,iC));
            }
        }

        double noiseMEAN = 4.96792e-08;
        
        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "calculating tensor...", 0, m_iSlices, true);

        for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
        {
            byte[][] aabSliceData = new byte[m_iWeights][ ];

            for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
            {
                File kFile = new File( m_aakDWIList[ iSlice ][iWeight] );
                if ( !kFile.exists() || !kFile.canRead() )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                    return;
                }
                int iLength = (int)kFile.length();
                aabSliceData[iWeight] = new byte[iLength];
                if ( iLength <= 0 )
                {
                    MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                    return;
                }
                try {
                    FileInputStream kFileReader = new FileInputStream(kFile);
                    kFileReader.read(aabSliceData[iWeight],0,iLength);
                    kFileReader.close();
                } catch (IOException e ) {}
            }

            for ( int iY = 0; iY < m_iYDim; iY++ )
            {
                for ( int iX = 0; iX < m_iXDim; iX++ )
                {
                    int iIndex = (iY * m_iXDim) + iX;
                    int index = iSlice * (m_iYDim * m_iXDim) + (iY * m_iXDim) + iX;
                    if ( kMaskImage.getBoolean( index ) )
                    {
                        Matrix SIGMA = Matrix.identity(vol, vol);
                        double[][] x = new double[vol][1];
                        Matrix X = new Matrix(x);
                        int[] r = new int[vol];
                        int[] c = new int[vol];
                        int idx=0;

                        for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
                        {
                            double p = (double)readFloat( aabSliceData[iWeight], iIndex );

                            //SIGMA is a diagonal matrix and its inverse would be diag(1/S(i,i))
                            if ( p<noiseMEAN )
                            {
                                p=noiseMEAN;
                            }
                        
                            double w = aaafWeights[iWeight][iSlice][iIndex];
                            if (w>0.196)
                            {
                                r[idx]=iWeight;
                                c[idx]=iWeight;
                                idx++;
                            }
                            X.set(iWeight,0,Math.log(p));
                            SIGMA.set(iWeight,iWeight,(p*p*w)); // SIGMA here becomes SIGMA.inverse

                        }
                        Matrix B2 = B.getMatrix(r, 0, 6+a0-1);
                        Matrix SIGMA2 = SIGMA.getMatrix(r, c);
                        Matrix X2 = X.getMatrix(r, 0, 0);
                        //Matrix A = ((B.transpose()).times( SIGMA )).times(B);
                        //Matrix Y = ((B.transpose()).times( SIGMA )).times(X);
                        Matrix A = ((B2.transpose()).times( SIGMA2 )).times(B2);
                        Matrix Y = ((B2.transpose()).times( SIGMA2 )).times(X2);
                        
                        Matrix D = new Matrix(7,1);
                        SingularValueDecomposition SVD = new SingularValueDecomposition(A);
                        Matrix S = SVD.getS();
                        for (int i=0; i<S.getRowDimension(); i++)
                            S.set(i,i,1/(S.get(i,i)));
                        D = (((SVD.getV()).times(S)).times(SVD.getU().transpose())).times(Y);
                    
                        // D = [Dxx, Dxy, Dxz, Dyy, Dyz, Dzz, Amplitude] 
                        float[] tensor = new float[10+vol];
                        for (int i=0; i<6; i++) { 
                            tensor[i] = (float)(-D.get(i,0)*1000000); // um^2/sec
                            if (i==0 && tensor[0]<0) { tensor[0]=(float)0.01; }
                            if (i==3 && tensor[3]<0) { tensor[3]=(float)0.01; }
                            if (i==5 && tensor[5]<0) { tensor[5]=(float)0.01; }
                        }
                    

                        float[] newTensor = new float[6];
                        newTensor[0] = tensor[0];
                        newTensor[1] = tensor[3];
                        newTensor[2] = tensor[5];
                        newTensor[3] = tensor[1];
                        newTensor[4] = tensor[2];
                        newTensor[5] = tensor[4];
                        for ( int iT = 0; iT < 6; iT++ )
                        {
                            afTensorData[index + iT*iLen] = newTensor[iT];
                        }
                    }
                    else
                    {
                        for ( int iT = 0; iT < 6; iT++ )
                        {
                            afTensorData[index + iT*iLen] = 0;
                        }
                    }
                }
            }
            kProgressBar.updateValue(iSlice+1);
            aabSliceData = null;

        }
        kProgressBar.dispose();
        aaafWeights = null;

        int[] extents = new int[]{m_iXDim, m_iYDim, m_iSlices, 6};
        m_kDTIImage = new ModelImage( ModelStorageBase.FLOAT, extents, new String( "DiffusionTensorImage" ) );
        try {
            m_kDTIImage.importData(0, afTensorData, true );
        } catch (IOException e) {}
        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));

        processDTI();
    }

    /** Translates the byte[] into float values at the given indes iIndex.
     * @param abData, byte[] containing float values.
     * @param iIndex, index into the array to get the float from.
     * @return float value representing 4 bytes starting at abData[iIndex*4].
     */
    private float readFloat( byte[] abData, int iIndex )
    {
        int b1 = abData[iIndex*4 + 0] & 0xff;
        int b2 = abData[iIndex*4 + 1] & 0xff;
        int b3 = abData[iIndex*4 + 2] & 0xff;
        int b4 = abData[iIndex*4 + 3] & 0xff;
        int tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        float fValue = Float.intBitsToFloat(tmpInt);
        return fValue;
    }


    /**
     * Launches the JFileChooser for the user to select the Diffusion Tensor Image. Loads the tensor data.
     */
    public void loadDTIFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            FileIO fileIO = new FileIO();
            m_kDTIImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            if(m_kDTIImage.getNDims() != 4) {
                MipavUtil.displayError("Diffusion Tensor file does not have correct dimensions");
                if(m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            if(m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil.displayError("Diffusion Tensor does not have correct dimensions");
                if(m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                m_kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            m_kDTIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            m_kTractOutputPath.setText(chooser.getSelectedFile().getAbsolutePath() + "_tract" );
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());            
        }
    }



    /**
     * Launches the JFileChooser for the user to select the Eigen Vector file.. Loads the eigen vector data.
     */
    public void loadEigenVectorFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose eigenvector file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            FileIO fileIO = new FileIO();
            if(m_kEigenVectorImage != null) {
                m_kEigenVectorImage.disposeLocal();
                m_kEigenVectorImage = null;
            }
            m_kEigenVectorImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            if(m_kEigenVectorImage.getNDims() != 4) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if(m_kEigenVectorImage != null) {
                    m_kEigenVectorImage.disposeLocal();
                }
                m_kEigenVectorPath.setText("");
                m_kEigenVectorImage = null;
                return;
            }
            if(m_kEigenVectorImage.getExtents()[3] != 9) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if(m_kEigenVectorImage != null) {
                    m_kEigenVectorImage.disposeLocal();
                }
                m_kEigenVectorPath.setText("");
                m_kEigenVectorImage = null;
                return;
            }
            m_kEigenVectorPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            int[] dimExtentsLUT;
            dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            m_kLUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            m_kLUTa.resetTransferLine(0.0f, (int) Math.round(m_kEigenVectorImage.getMin()), 255.0f, (int) Math.round(m_kEigenVectorImage.getMax()));
            int[] extents;
            extents = new int[4];
            extents[0] = Math.round(m_kEigenVectorImage.getExtents()[0]);
            extents[1] = Math.round(m_kEigenVectorImage.getExtents()[1]);
            extents[2] = Math.round(m_kEigenVectorImage.getExtents()[2]);
            extents[3] = Math.round(m_kEigenVectorImage.getExtents()[3]);   
        }
    }
	
	
    /**
     * Launches the JFileChooser for the user to select the functional anisotropy file. Loads the anisotropy data.
     */
    public void loadAnisotropyFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose anisotropy file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            FileIO fileIO = new FileIO();
            if(m_kAnisotropyImage != null) {
                m_kAnisotropyImage.disposeLocal();
                m_kAnisotropyImage = null;
            }
            m_kAnisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            if(m_kAnisotropyImage.getNDims() > 3) {
                MipavUtil.displayError("anisotropy file does not have correct dimensions");
                if(m_kAnisotropyImage != null) {
                    m_kAnisotropyImage.disposeLocal();
                }
                m_kAnisotropyPath.setText("");
                m_kAnisotropyImage = null;
                return;
            }
            m_kAnisotropyPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }


    /** 
     * Calculates the eigen vector data from the dtiImage.
     * @param dtiImage, Diffusion Tensor image.
     */
    private void calcEigenVector( ModelImage dtiImage )
    {
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];
        int iZDim = dtiImage.getExtents()[2];
        int iSliceSize = dtiImage.getExtents()[0] * dtiImage.getExtents()[1];
        float[] afData = new float[iLen];
        float[] afDataCM = new float[iLen*9];

        float[] afTensorData = new float[6];
        Vector3f kV1 = new Vector3f( Vector3f.ZERO );
        Vector3f kV2 = new Vector3f( Vector3f.ZERO );
        Vector3f kV3 = new Vector3f( Vector3f.ZERO );

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating Eigen Vectors ",
                                                             "calculating eigen vectors...", 0, iZDim, true);

        for ( int i = 0; i < iLen; i++ )
        {
            boolean bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = dtiImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                Matrix3f kMatrix = new Matrix3f( afTensorData[0], afTensorData[3], afTensorData[4],
                                                 afTensorData[3], afTensorData[1], afTensorData[5], 
                                                 afTensorData[4], afTensorData[5], afTensorData[2] );
                Matrix3f kEigenValues = new Matrix3f();
                Matrix3f.EigenDecomposition( kMatrix, kEigenValues );
                float fLambda1 = kEigenValues.GetData(2,2);
                float fLambda2 = kEigenValues.GetData(1,1);
                float fLambda3 = kEigenValues.GetData(0,0);
                kV1 = kMatrix.GetColumn(2);
                kV2 = kMatrix.GetColumn(1);
                kV3 = kMatrix.GetColumn(0);

                afData[i] = (float)(Math.sqrt(1.0/2.0) *
                                    ( ( Math.sqrt( (fLambda1 - fLambda2)*(fLambda1 - fLambda2) +
                                                   (fLambda2 - fLambda3)*(fLambda2 - fLambda3) +
                                                   (fLambda3 - fLambda1)*(fLambda3 - fLambda1)   ) ) /
                                      ( Math.sqrt( fLambda1*fLambda1 + fLambda2*fLambda2 + fLambda3*fLambda3 ) ) ) );
            }
            else
            {
                afData[i] = 0;
            }

            afDataCM[i + 0*iLen] = kV1.X();
            afDataCM[i + 1*iLen] = kV1.Y();
            afDataCM[i + 2*iLen] = kV1.Z();

            afDataCM[i + 3*iLen] = kV2.X();
            afDataCM[i + 4*iLen] = kV2.Y();
            afDataCM[i + 5*iLen] = kV2.Z();

            afDataCM[i + 6*iLen] = kV3.X();
            afDataCM[i + 7*iLen] = kV3.Y();
            afDataCM[i + 8*iLen] = kV3.Z();

            if ( (i%iSliceSize) == 0 )
            {
                kProgressBar.updateValue((i/iSliceSize)+1);
            }
        }
        kProgressBar.dispose();
    
        int[] extentsEV = new int[]{dtiImage.getExtents()[0], dtiImage.getExtents()[1], dtiImage.getExtents()[2], 9};
        int[] extentsA = new int[]{dtiImage.getExtents()[0], dtiImage.getExtents()[1], dtiImage.getExtents()[2]};


        m_kAnisotropyImage = new ModelImage( ModelStorageBase.FLOAT, extentsA, new String( dtiImage.getFileInfo(0).getFileName() + "FA") );
        try {
            m_kAnisotropyImage.importData(0, afData, true);
        } catch (IOException e) { }

        m_kEigenVectorImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, extentsEV, new String( dtiImage.getFileInfo(0).getFileName() + "EG") );
        try {
            m_kEigenVectorImage.importData(0, afDataCM, true);
        } catch (IOException e) { }

        //Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        int[] dimExtentsLUT;
        dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        m_kLUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
        m_kLUTa.resetTransferLine(0.0f, (int) Math.round(m_kEigenVectorImage.getMin()), 255.0f, (int) Math.round(m_kEigenVectorImage.getMax()));
        int[] extents;
        extents = new int[4];
        extents[0] = Math.round(m_kEigenVectorImage.getExtents()[0]);
        extents[1] = Math.round(m_kEigenVectorImage.getExtents()[1]);
        extents[2] = Math.round(m_kEigenVectorImage.getExtents()[2]);
        extents[3] = Math.round(m_kEigenVectorImage.getExtents()[3]);   

        if ( (m_kReconstructTracts != null) && m_kReconstructTracts.isSelected() )
        {
	    reconstructTracts( dtiImage, m_kEigenVectorImage );
	}
    }

    /** Creates the user-interface for the Diffusion Tensor Image dialog.
     * @return JPanel containing the user-interface for the Diffusion Tensor Image dialog.
     */
    private JPanel createDTIPanel()
    {
        GridBagLayout kGBL  = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        JPanel kDTIPanel = new JPanel(kGBL);
        JPanel kDTIFilesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);
        JLabel kDTILabel = new JLabel("  Diffusion Tensor Image: ");
        kDTIFilesPanel.add(kDTILabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kDTIPath = new JTextField(35);
        m_kDTIPath.setEditable(false);
        m_kDTIPath.setBackground(Color.white);
        kDTIFilesPanel.add(m_kDTIPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        JButton kDTIBrowseButton = new JButton("Browse");
        kDTIBrowseButton.addActionListener(this);
        kDTIBrowseButton.setActionCommand("DTIBrowse");
        kDTIFilesPanel.add(kDTIBrowseButton, gbc);

	m_kReconstructTracts = new JCheckBox("Tract Reconstruction");
	m_kReconstructTracts.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        kDTIFilesPanel.add(m_kReconstructTracts, gbc);

	m_kTractOutputPath = new JTextField(35);
        m_kTractOutputPath.setEditable(true);
        m_kTractOutputPath.setBackground(Color.white);
        gbc.gridx++;
        kDTIFilesPanel.add(m_kTractOutputPath, gbc);

        gbc.insets = new Insets(0,0,0,0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kDTIPanel.add(kDTIFilesPanel, gbc);
        return kDTIPanel;
    }
    
    /** Creates the user-interface for the EigenVector FA dialog.
     * @return JPanel containing the user-interface for the EigenVector FA dialog.
     */
    private JPanel createEigenPanel()
    {
        GridBagLayout kGBL  = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        JPanel kEigenPanel = new JPanel(kGBL);
        JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);
        JLabel eigenvectorLabel = new JLabel(" eigenvector file: ");
        filesPanel.add(eigenvectorLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kEigenVectorPath = new JTextField(35);
        m_kEigenVectorPath.setEditable(false);
        m_kEigenVectorPath.setBackground(Color.white);
        filesPanel.add(m_kEigenVectorPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        JButton eigenvectorBrowseButton = new JButton("Browse");
        eigenvectorBrowseButton.addActionListener(this);
        eigenvectorBrowseButton.setActionCommand("eigenvectorBrowse");
        filesPanel.add(eigenvectorBrowseButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel anisotropyLabel = new JLabel(" anisotropy file: ");
        filesPanel.add(anisotropyLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        m_kAnisotropyPath = new JTextField(35);
        m_kAnisotropyPath.setEditable(false);
        m_kAnisotropyPath.setBackground(Color.white);
        filesPanel.add(m_kAnisotropyPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 1;
        JButton anisotropyBrowseButton = new JButton("Browse");
        anisotropyBrowseButton.addActionListener(this);
        anisotropyBrowseButton.setActionCommand("anisotropyBrowse");
        filesPanel.add(anisotropyBrowseButton, gbc);

        gbc.insets = new Insets(0,0,0,0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kEigenPanel.add(filesPanel, gbc);
        return kEigenPanel;
    }

    /** Creates the user-interface for the Fiber Bundle Tract dialog.
     * @return JPanel containing the user-interface for the Fiber Bundle Tract dialog.
     */
    private JPanel createTractPanel()
    {
        GridBagLayout kGBL  = new GridBagLayout();
        GridBagConstraints gbc = new GridBagConstraints();
        JPanel kTractPanel = new JPanel(kGBL);

        JPanel kParamsPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel kNumberTractsLimit = new JLabel( "Maximum number of tracts to display:" );
        kParamsPanel.add(kNumberTractsLimit, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kTractsLimit = new JTextField("100", 5);
        m_kTractsLimit.setBackground(Color.white);
        kParamsPanel.add(m_kTractsLimit, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMinLength = new JLabel( "Minimum tract length:" );
        kParamsPanel.add(m_kTractsMinLength, gbc);
        gbc.gridx++;
        m_kTractsMin = new JTextField("50", 5 );
        m_kTractsMin.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMin, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel m_kTractsMaxLength = new JLabel( "Maximum tract length:" );
        kParamsPanel.add(m_kTractsMaxLength, gbc);
        gbc.gridx++;
        m_kTractsMax = new JTextField("100", 5 );
        m_kTractsMax.setBackground(Color.white);
        kParamsPanel.add(m_kTractsMax, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kUseVOI = new JLabel( "Use VOI:" );
        kParamsPanel.add(kUseVOI, gbc);
        gbc.gridx++;
        m_kUseVOICheck = new JCheckBox("use voi" );
        kParamsPanel.add(m_kUseVOICheck, gbc);

        
        JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);
        JLabel kTractLabel = new JLabel(" DTI tract file: ");
        filesPanel.add(kTractLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kTractPath = new JTextField(35);
        m_kTractPath.setEditable(false);
        m_kTractPath.setBackground(Color.white);
        filesPanel.add(m_kTractPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        JButton kTractBrowseButton = new JButton("Browse");
        kTractBrowseButton.addActionListener(this);
        kTractBrowseButton.setActionCommand("tractBrowse");
        filesPanel.add(kTractBrowseButton, gbc);

        gbc.insets = new Insets(0,0,0,0);
        gbc.gridx = 0;
        gbc.gridy = 0;
        kTractPanel.add(kParamsPanel, gbc);
        gbc.gridy++;
        kTractPanel.add(filesPanel, gbc);
        return kTractPanel;
    }

    /** Creates the user-interface for the Diffusion Weighted Images dialog.
     * @return JPanel containing the user-interface for the Diffusion Weighted Images dialog.
     */
    private JPanel createDWIPanel()
    {
        GridBagLayout kGBL  = new GridBagLayout();
        JLabel dim1;
        JLabel dim2;
        JLabel dim3;
        JLabel dim4;

        JPanel kDWIPanel = new JPanel();
        kDWIPanel.setLayout( new GridLayout(2,1) );
        JPanel kDWIFilesPanel = new JPanel(kGBL);

        GridBagConstraints gbcPanelDims = new GridBagConstraints();
        JPanel panelDims = new JPanel(new GridBagLayout());
        panelDims.setBorder(buildTitledBorder("Dimensions & resolutions"));

        gbcPanelDims.insets = new Insets(5, 5, 5, 5);
        gbcPanelDims.fill = GridBagConstraints.NONE;

        dim1 = new JLabel("1st");
        dim1.setFont(serif12);
        dim1.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 0;
        panelDims.add(dim1, gbcPanelDims);

        m_kTextDim1 = new JTextField(5);
        m_kTextDim1.setText("128");
        m_kTextDim1.setFont(serif12);
        m_kTextDim1.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 0;
        panelDims.add(m_kTextDim1, gbcPanelDims);

        m_kTextRes1 = new JTextField(5);
        m_kTextRes1.setText("1.0");
        m_kTextRes1.setFont(serif12);
        m_kTextRes1.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 0;
        panelDims.add(m_kTextRes1, gbcPanelDims);

        dim2 = new JLabel("2nd");
        dim2.setFont(serif12);
        dim2.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 1;
        panelDims.add(dim2, gbcPanelDims);

        m_kTextDim2 = new JTextField(5);
        m_kTextDim2.setText("157");
        m_kTextDim2.setFont(serif12);
        m_kTextDim2.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 1;
        panelDims.add(m_kTextDim2, gbcPanelDims);

        m_kTextRes2 = new JTextField(5);
        m_kTextRes2.setText("1.0");
        m_kTextRes2.setFont(serif12);
        m_kTextRes2.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 1;
        panelDims.add(m_kTextRes2, gbcPanelDims);

        dim3 = new JLabel("3rd");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 2;
        panelDims.add(dim3, gbcPanelDims);

        m_kTextDim3 = new JTextField(5);
        m_kTextDim3.setText("114");
        m_kTextDim3.setFont(serif12);
        m_kTextDim3.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 2;
        panelDims.add(m_kTextDim3, gbcPanelDims);

        m_kTextRes3 = new JTextField(5);
        m_kTextRes3.setText("1.0");
        m_kTextRes3.setFont(serif12);
        m_kTextRes3.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 2;
        panelDims.add(m_kTextRes3, gbcPanelDims);

        dim4 = new JLabel("4th");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 3;
        panelDims.add(dim4, gbcPanelDims);

        m_kTextDim4 = new JTextField(5);
        m_kTextDim4.setText("72");
        m_kTextDim4.setFont(serif12);
        m_kTextDim4.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 3;
        panelDims.add(m_kTextDim4, gbcPanelDims);

        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 5;
        gbcPanelDims.gridwidth = 3;
        gbcPanelDims.fill = GridBagConstraints.VERTICAL;
        gbcPanelDims.weighty = 1;
        panelDims.add(new JPanel(), gbcPanelDims);
        
        JPanel kDimsInstructions = new JPanel();
        kDimsInstructions.setLayout(new GridLayout( 1, 1 ));
        kDimsInstructions.setBorder(buildTitledBorder("1). Set the image dimensions and resoultions"));
        kDimsInstructions.add(panelDims);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);

        JLabel kBMatrixLabel = new JLabel("  BMatrix file: ");
        kDWIFilesPanel.add(kBMatrixLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        m_kBMatrixPath = new JTextField(35);
        m_kBMatrixPath.setEditable(false);
        m_kBMatrixPath.setBackground(Color.white);
        kDWIFilesPanel.add(m_kBMatrixPath, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        JButton kBMatrixBrowseButton = new JButton("Browse");
        kBMatrixBrowseButton.addActionListener(this);
        kBMatrixBrowseButton.setActionCommand("BMatrixBrowse");
        kDWIFilesPanel.add(kBMatrixBrowseButton, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kDWILabel = new JLabel("  Diffusion Wieghted Image (.path): ");
        kDWIFilesPanel.add(kDWILabel, gbc);
        gbc.gridx = 1;
        m_kDWIPath = new JTextField(35);
        m_kDWIPath.setEditable(false);
        m_kDWIPath.setBackground(Color.white);
        kDWIFilesPanel.add(m_kDWIPath, gbc);
        gbc.gridx = 2;
        JButton kDWIBrowseButton = new JButton("Browse");
        kDWIBrowseButton.addActionListener(this);
        kDWIBrowseButton.setActionCommand("DWIBrowse");
        kDWIFilesPanel.add(kDWIBrowseButton, gbc);
        
	m_kReconstructTracts = new JCheckBox("Tract Reconstruction");
	m_kReconstructTracts.setSelected(false);
        gbc.gridx = 0;
        gbc.gridy++;
        kDWIFilesPanel.add(m_kReconstructTracts, gbc);

	m_kTractOutputPath = new JTextField(35);
        m_kTractOutputPath.setEditable(true);
        m_kTractOutputPath.setBackground(Color.white);
        gbc.gridx++;
        kDWIFilesPanel.add(m_kTractOutputPath, gbc);

        JPanel kFileInstructions = new JPanel();
        kFileInstructions.setLayout(new GridLayout( 1, 1 ));
        kFileInstructions.setBorder(buildTitledBorder("2). Select the BMatrix and .path files"));
        kFileInstructions.add(kDWIFilesPanel);
        
        kDWIPanel.add(kDimsInstructions);
        kDWIPanel.add(kFileInstructions);
        return kDWIPanel;
    }

    /** Constructs the Fiber Bundle Tracts from the dtiImage and the
     * eigenImage parameters. The fiber bundles are output to a file
     * sepecified by the user.
     * @param dtiImage, Diffusion Tensor Image.
     * @param eigenImage, EigenVector Image.
     */
    private void reconstructTracts( ModelImage dtiImage, ModelImage eigenImage )
    {
        m_kDTIImage = dtiImage;
        m_kEigenVectorImage = eigenImage;
        // save the file version to disk
        File kFile = new File(m_kTractOutputPath.getText());
        FileOutputStream kFileWriter = null;
        try {
            kFileWriter = new FileOutputStream(kFile);
        } catch ( FileNotFoundException e1 ) {
            kFileWriter = null;
        }

        int iDimX = m_kDTIImage.getExtents()[0];
        int iDimY = m_kDTIImage.getExtents()[1];
        int iDimZ = m_kDTIImage.getExtents()[2];
        int iLen = m_kDTIImage.getExtents()[0] * m_kDTIImage.getExtents()[1] * m_kDTIImage.getExtents()[2];

        float[] afVectorData = new float[3];

        int iCount = 0;
        int iTractSize = 0;

        ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating Fiber Bundle Tracts ", "calculating tracts...", 0, iDimZ, true);

        for ( int iZ = 0; iZ < iDimZ; iZ++ )
        {
            for ( int iY = 0; iY < iDimY; iY++ )
            {
                for ( int iX = 0; iX < iDimX; iX++ )
                {
                    int i = iZ * (iDimY*iDimX) + iY * iDimX + iX;

                    boolean bAllZero = true;
                    for ( int j = 0; j < 3; j++ )
                    {
                        afVectorData[j] = m_kEigenVectorImage.getFloat(i + j*iLen);
                        if ( afVectorData[j] != 0 )
                        {
                            bAllZero = false;
                        }

                    }
                    if ( !bAllZero )
                    {
                        Vector3f kPos = new Vector3f( iX, iY, iZ );
                        Vector<Integer> kTract = new Vector<Integer>();
                        kTract.add(i);

                        Vector4f kV1 = new Vector4f( afVectorData[0], afVectorData[1], afVectorData[2], 0 );
                        Vector4f kV2 = kV1.neg();

                        kV1.Normalize();
                        kV2.Normalize();
                       
                        kV1.W(i);
                        kV2.W(i);
                        Vector<Vector4f> kVisited = new Vector<Vector4f>();
                        kVisited.add( kV1 );
                        kVisited.add( kV2 );

                        
                        traceTract( kTract, kPos, kV1, m_kDTIImage, true, kVisited );
                        traceTract( kTract, kPos, kV2, m_kDTIImage, false, kVisited );

                        if ( kTract.size() > 1 )
                        {
                            iCount++;
                            iTractSize += kTract.size();
                            System.err.println( iX + " " + iY + " " + iZ + " tract size " + kTract.size() );

                            outputTract( kTract, iDimX, iDimY, iDimZ, kFileWriter );
                        }
                        kTract.clear();
                        kTract = null;

                        int iVQuantity = kVisited.size();
                        for (int iP = 0; iP < iVQuantity; iP++)
                        {
                            kVisited.get(iP).finalize();
                        }
                        kVisited.clear();
                        kVisited = null;

                    }
                }
            }
            kProgressBar.updateValue(iZ+1);
        }
        kProgressBar.dispose();
        System.err.println( "Number of tracts: " + iCount + " tract size " + iTractSize );

        try {
            kFileWriter.close();
        } catch ( IOException e2 ) {}
    }

    /** Traces a single fiber bundle tract starting at the input
     * position and following the input direction.
     * @param kTract, fiber bundle tract, new positions are stored in this tract as the fiber is traced.
     * @param kStart, starting positon of the tract.
     * @param kDir, direction from the position.
     * @param dtiImage, Diffusion Tensor image used to calculate next direction of tract.
     * @param bDir, boolean when true the positions are added to the
     * end of the tract (positive direction). When false the positions
     * are added to the beginning of the tract (negative direction).
     */
    private void traceTract( Vector<Integer> kTract, Vector3f kStart, Vector4f kDir,
                             ModelImage dtiImage, boolean bDir,
                             Vector<Vector4f> kVisited )
    {
        int iDimX = dtiImage.getExtents()[0];
        int iDimY = dtiImage.getExtents()[1];
        int iDimZ = dtiImage.getExtents()[2];
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];

        float[] afTensorData = new float[6];
        float[] afVectorData = new float[3];

        boolean bDone = false;
        Matrix3f kMatrix = new Matrix3f();
        Vector3f kOut;
        Vector3f kNext;
        int iX;
        int iY;
        int iZ;
        int i;
        boolean bAllZero = true;

        while ( !bDone )
        {
            kNext = kStart.add( new Vector3f(kDir.X(), kDir.Y(), kDir.Z()) );
            iX = Math.round(kNext.X());
            iY = Math.round(kNext.Y());
            iZ = Math.round(kNext.Z());
            i = iZ * (iDimY*iDimX) + iY * iDimX + iX;
            
            if ( (iZ < 0) || (iZ >= iDimZ) ||
                 (iY < 0) || (iY >= iDimY) ||
                 (iX < 0) || (iX >= iDimX)  )
            {
                return;
            }

            bAllZero = true;
            for ( int j = 0; j < 6; j++ )
            {
                afTensorData[j] = dtiImage.getFloat(i + j*iLen);
                if ( afTensorData[j] != 0 )
                {
                    bAllZero = false;
                }
            }
            if ( !bAllZero )
            {
                kMatrix.SetData( afTensorData[0], afTensorData[3], afTensorData[4],
                                 afTensorData[3], afTensorData[1], afTensorData[5], 
                                 afTensorData[4], afTensorData[5], afTensorData[2] );
                
                kOut = kMatrix.mult( new Vector3f(kDir.X(), kDir.Y(), kDir.Z()) );
                kOut.Normalize();
                Vector4f kOut4 = new Vector4f( kOut.X(), kOut.Y(), kOut.Z(), i);

                if ( contains(kVisited, kOut4) )
                {
                    return;
                }
                kVisited.add( kOut4 );
                
                if ( bDir )
                {
                    kTract.add( i );
                }
                else
                {
                    kTract.add( 0, i );
                }

                kStart = kNext;
                kDir = kOut4;
            }
            else
            {
                bDone = true;
            }
        }
    }

    /** Determines if the kVisited list of positions contains the input Vector4f kV.
     * @param kVisited, vector of Vector4f positions.
     * @param kV, input vector to test.
     * @return true if kVisited contains a Vector4f with values equal to kV. False otherwise.
     */
    private boolean contains( Vector<Vector4f> kVisited, Vector4f kV )
    {
        for ( int i = 0; i < kVisited.size(); i++ )
        {
            if ( kVisited.get(i).isEqual(kV) )
            {
                return true;
            }
        }
        return false;
    }

    /** Determines if the input tract is contained within the ModelImage representing user-selected VOIs.
     * @param kVOIImage user-selected VOI image.
     * @param kTract, list of voxels in the current fiber bundle tract.
     * @return true if the tract passes through the VOI or if the VOIImage is null, false otherwise.
     */
    private boolean contains( ModelImage kVOIImage, Vector<Integer> kTract )
    {
        if ( kVOIImage == null )
        {
            return true;
        }
        for ( int i = 0; i < kTract.size(); i++ )
        {
            int iIndex = kTract.get(i);
            if ( kVOIImage.getBoolean(iIndex) )
            {
                return true;
            }
        }
        return false;
    }

    /** Writes the fiber bundle tract to disk.
     * @param kTract, the fiber bundle tract.
     * @param iDimX, x-dimension of the diffusion tensor image.
     * @param iDimY, y-dimension of the diffusion tensor image.
     * @param iDimZ, z-dimension of the diffusion tensor image.
     * @param kFileWrite, FileOutputStream.
     */
    private void outputTract( Vector<Integer> kTract, int iDimX, int iDimY, int iDimZ,
			      FileOutputStream kFileWriter )
    {
        int iVQuantity = kTract.size();

        int iBufferSize = iVQuantity*4 + 4;
        if ( m_bFirstWrite )
        {
            iBufferSize += 3*4;
        }
        ByteArrayOutputStream acBufferOut = new ByteArrayOutputStream( iBufferSize );
        DataOutputStream acDataOut = new DataOutputStream( acBufferOut );
        if ( kFileWriter != null )
        {
            try {
		if ( m_bFirstWrite )
		{
		    acDataOut.writeInt(iDimX);
		    acDataOut.writeInt(iDimY);
		    acDataOut.writeInt(iDimZ);
		    m_bFirstWrite = false;
		}
                acDataOut.writeInt(iVQuantity);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        for (int i = 0; i < iVQuantity; i++)
        {
            if ( kFileWriter != null )
            {
                try {
                    acDataOut.writeInt(kTract.get(i));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }

        if ( kFileWriter != null )
        {
            byte[] acBuffer = acBufferOut.toByteArray();
            try {
                kFileWriter.write(acBuffer,0,iBufferSize);
            } catch ( IOException e2 ) {
                acBuffer = null;
            }
            acBuffer = null;
        }
        try {
            acBufferOut.close();
            acDataOut.close();
        } catch (IOException e) {}
        acBufferOut = null;
        acDataOut = null;
    }

    /** Reads a single fiber bundle tract from disk.
     * @param kFileReader, FileInputStream.
     * @return Vector<Integer> fiber bundle tract -- list of voxel
     * indices in order in which they appear in the tract.
     */
    private Vector<Integer> inputTract( FileInputStream kFileReader )
    {
        int iVQuantity = 0;
        int iBufferSize = 4;

        byte[] racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer,0,iBufferSize);
        } catch (IOException e1) {}
        ByteArrayInputStream acBufferIn = new ByteArrayInputStream( racBuffer );
        DataInputStream acDataIn = new DataInputStream( acBufferIn );
        try {
            iVQuantity = acDataIn.readInt();
        } catch (IOException e) {
            e.printStackTrace();
        }
        acBufferIn = null;
        acDataIn = null;
        racBuffer = null;

        iBufferSize = 4 * iVQuantity;
        racBuffer = new byte[iBufferSize];
        try {
            kFileReader.read(racBuffer,0,iBufferSize);
        } catch (IOException e1) {}
        acBufferIn = new ByteArrayInputStream( racBuffer );
        acDataIn = new DataInputStream( acBufferIn );

        Vector<Integer> kTract = new Vector<Integer>();
        for (int i = 0; i < iVQuantity; i++)
        {
            try {
                kTract.add( acDataIn.readInt() );
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        acBufferIn = null;
        acDataIn = null;

        return kTract;
    }

    /** Launches the JFileChooser for the user to select the tract
     * file. Stores the File for the tract file but does not read the
     * file. */
    private void loadTractFile()
    {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Tensor Tract file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            m_kTractFile = new File(chooser.getSelectedFile().getAbsolutePath());
            if ( !m_kTractFile.exists() || !m_kTractFile.canRead() )
            {
                m_kTractFile = null;
                return;
            }
            int iLength = (int)m_kTractFile.length();
            if ( iLength <= 0 )
            {
                m_kTractFile = null;
                return;
            }
            m_kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());            
        }
    }

    /** 
     * process the tract file. Uses the File stored from the
     * loadTractFile fn.  Loads fiber bundle tracts, filters them with
     * the user-defined display parameters, and passes them to the
     * GPUVolumeRender for display.
     */
    private void processTractFile()
    {
        if ( m_kTractFile == null )
        {
            MipavUtil.displayError("Tract file must be set.");
            return;
        }

        try {
            int iNumTractsLimit = (new Integer( m_kTractsLimit.getText() )).intValue() ;
            int iTractMinLength = (new Integer( m_kTractsMin.getText() )).intValue() ;
            int iTractMaxLength = (new Integer( m_kTractsMax.getText() )).intValue() ;

            int iNumTracts = 0;

            ModelImage kVOIImage = null;
            if ( m_kUseVOICheck.isSelected() )
            {
                kVOIImage = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage().generateBinaryImage(false, true);
            }


            int iDimX = 0, iDimY = 0, iDimZ = 0;
            FileInputStream kFileReader = new FileInputStream(m_kTractFile);
            int iBufferSize = 3*4;

            byte[] racBuffer = new byte[iBufferSize];
            kFileReader.read(racBuffer,0,iBufferSize);
            ByteArrayInputStream acBufferIn = new ByteArrayInputStream( racBuffer );
            DataInputStream acDataIn = new DataInputStream( acBufferIn );
            try {
                iDimX = acDataIn.readInt();
                iDimY = acDataIn.readInt();
                iDimZ = acDataIn.readInt();
            } catch (IOException e) {
                e.printStackTrace();
            }
            acBufferIn = null;
            acDataIn = null;
            racBuffer = null;

            int iLength = (int)m_kTractFile.length();
            int iBufferNext = iBufferSize;
            while (iBufferNext < iLength)
            {
                if ( iNumTracts >= iNumTractsLimit )
                {
                    break;
                }

                Vector<Integer> kTract = inputTract( kFileReader );
                iBufferNext += kTract.size() * 4 + 4;
                int iVQuantity = kTract.size();                   
                if ( contains( kVOIImage, kTract ) )
                {
                    if ( (iVQuantity > iTractMinLength) && (iVQuantity < iTractMaxLength) )
                    {
                        if ( iNumTracts < iNumTractsLimit )
                        {
                            iNumTracts++;
                            Attributes kAttr = new Attributes();
                            kAttr.SetPChannels(3);
                            kAttr.SetCChannels(0,3);
                            VertexBuffer pkVBuffer = new VertexBuffer(kAttr,iVQuantity);                        

                            for (int i = 0; i < iVQuantity; i++)
                            {
                                int iIndex = kTract.get(i);

                                int iX = iIndex % iDimX;
                                iIndex -= iX;
                                iIndex /= iDimX;
                                
                                int iY = iIndex % iDimY;
                                iIndex -= iY;
                                iIndex /= iDimY;
                                
                                int iZ = iIndex;
                                
                                float fX = (float)(iX)/(float)(iDimX);
                                float fY = (float)(iY)/(float)(iDimY);
                                float fZ = (float)(iZ)/(float)(iDimZ);
                                
                                pkVBuffer.Position3(i,
                                                    new Vector3f( (float)(fX-.5f), (float)(fY-.5f), (float)(fZ-.5f) ) );
                                pkVBuffer.Color3(0,i, new ColorRGB(fX, fY, fZ));

                            }
                            boolean bClosed = false;
                            boolean bContiguous = true;
                            m_kVolumeDisplay.addPolyline( new Polyline(pkVBuffer,bClosed,bContiguous) );
                        }
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /** ViewImageUpdateInterface : stub */
    public void setSlice(int slice) {}

    /** ViewImageUpdateInterface : stub */
    public void setTimeSlice(int tSlice) {}

    /** ViewImageUpdateInterface : stub */
    public boolean updateImageExtents() {
        return false;
    }

    /** ViewImageUpdateInterface : called when the JDialogBrainSurfaceExtractor finishes. Calls processDWI. */
    public boolean updateImages() {
        ModelImage kMaskImage = ViewUserInterface.getReference().getRegisteredImageByName(m_kBrainFrame.getComponentImage().commitPaintToMask());
        m_kBrainImage.removeImageDisplayListener(this);
        processDWI(kMaskImage);
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }
};