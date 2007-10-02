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
import gov.nih.mipav.view.WildMagic.ApplicationDemos.Polylines;
import gov.nih.mipav.view.WildMagic.ApplicationDemos.PolylinesThread;
import gov.nih.mipav.view.WildMagic.LibFoundation.Mathematics.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.SceneGraph.*;
import gov.nih.mipav.view.WildMagic.LibGraphics.ObjectSystem.*;
import gov.nih.mipav.view.WildMagic.LibFoundation.NumericalAnalysis.*;
import java.io.FileInputStream;
import gov.nih.mipav.model.structures.jama.*;

public class JDialogDTIInput extends JDialogBase
{

    public static final int DWI = 0;
    public static final int DTI = 1;
    public static final int EG_FA = 2;
    public static final int TRACTS = 3;

    /** eigenvector src image **/
    private ModelImage eigvecSrcImage;
    
    /** anisotropy src image **/
    private ModelImage anisotropyImage;

    /** LUT of input image **/
    private ModelLUT LUTa;

    private JTextField eigenvectorPath;
    private JTextField anisotropyPath;
    private JTextField kDTIPath;
    private JTextField kBMatrixPath;
    private JTextField kDWIPath;
    private JTextField kTractPath;

    /** DOCUMENT ME! */
    private JTextField textDim1; // dimensions

    /** DOCUMENT ME! */
    private JTextField textDim2;

    /** DOCUMENT ME! */
    private JTextField textDim3;

    /** DOCUMENT ME! */
    private JTextField textDim4;

    /** DOCUMENT ME! */
    private JTextField textRes1; // resolutions

    /** DOCUMENT ME! */
    private JTextField textRes2;

    /** DOCUMENT ME! */
    private JTextField textRes3;

    /** DOCUMENT ME! */
    private JTextField textRes4;

    //private GMatrixf[] m_akBMatrix = null;

    private GMatrixf m_kBMatrix = null;
    private String[][] m_aakDWIList = null;

    private int m_iSlices = 0;
    private int m_iWeights = 0;

    private int m_iXDim = 0;
    private int m_iYDim = 0;

    private int[][] m_aaiSeries = null;
    private int m_iSeriesCount = 0;
    private boolean[] m_abBZeroFlag = null;

    private int[] m_aiB0Index = null;

    private ModelImage m_kDTIImage = null;
    private ModelImage m_kEigenImage = null;
    private int m_iType;
        
    private Polylines m_kPolylineDisplay = null;
    private GPUVolumeRender m_kVolumeDisplay = null;

    private boolean m_bFirstWrite = true;

    private JCheckBox m_kReconstructTracts;
    private JTextField m_kTractOutputPath;

    private JTextField kTractsLimit;
    private JTextField kTractsMin;
    private JTextField kTractsMax;
    private File m_kTractFile = null;
    private JCheckBox kUseVOICheck;


    public JDialogDTIInput( int iType )
    {
        super();
        init( iType );
        m_iType = iType;
    }

    public JDialogDTIInput( int iType, GPUVolumeRender kDisplay ) 
    {
        super();
        init( iType );
        m_iType = iType;
        m_kVolumeDisplay = kDisplay;
    }

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

    public void actionPerformed(ActionEvent kAction) {
        Object source = kAction.getSource();
        String command = kAction.getActionCommand();

        if ( source == cancelButton )
        {
            dispose();
        }
        if(command.equalsIgnoreCase("eigenvectorBrowse")) {
            loadEigenVectorFile();
        }
        else if(command.equalsIgnoreCase("anisotropyBrowse")) {
            loadAnisotropyFile();
        }
        else if(command.equalsIgnoreCase("DTIBrowse")) {
            loadDTIFile();
        }
        else if(command.equalsIgnoreCase("DWIBrowse")) {
            loadDWIFile();
        }
        else if(command.equalsIgnoreCase("BMatrixBrowse")) {
            loadBMatrixFile();
        }
        else if(command.equalsIgnoreCase("tractBrowse")) {
            loadTractFile();
        }
        else if(command.equalsIgnoreCase("ok")) {
            if ( m_iType == DWI )
            {
                if ( m_kBMatrix == null || m_aakDWIList == null )
                {
                    MipavUtil.displayError("Both BMatrix and .path files are needed.");
                    return;
                }
                processDWI();
                new DialogDTIColorDisplay(eigvecSrcImage, anisotropyImage, LUTa, false);
                dispose();
            }
            else if ( m_iType == DTI )
            {
                if( m_kDTIImage == null ) {
                    MipavUtil.displayError("Diffusion Tensor Image needed");
                    return;
                }
                processDTI();
                new DialogDTIColorDisplay(eigvecSrcImage, anisotropyImage, LUTa, false);
                dispose();
            }
            else if ( m_iType == EG_FA )
            {
                if(eigvecSrcImage == null || anisotropyImage == null) {
                    MipavUtil.displayError("Both eigenvector and anisotropy files are needed");
                    return;
                }
                new DialogDTIColorDisplay(eigvecSrcImage, anisotropyImage, LUTa, false);
                dispose();
            }
            else if ( m_iType == TRACTS )
            {
                processTractFile();
                dispose();
                return;
            }
        }
    }

    /**
     * browses and loads DTI file
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
            kBMatrixPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            try {
                BufferedReader in = new BufferedReader(new FileReader(kFile));
                String str;

                m_iWeights = Integer.valueOf(textDim4.getText()).intValue();
                GMatrixf kBMatrix = new GMatrixf( m_iWeights, 6 );
                //GMatrixf kBMatrix = new GMatrixf( m_iWeights, 6 );
                m_abBZeroFlag = new boolean[ m_iWeights ];
                for ( int iRow = 0; iRow < m_iWeights; iRow++ )
                {
                    str = in.readLine();
                    //System.err.print(str + " ==> " );
                    java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                    boolean bZero = true;
                    for ( int iCol = 0; iCol < 6; iCol++ )
                    {
                        float fValue = Float.valueOf(st.nextToken()).floatValue();
                        //System.err.print(fValue + " " );
                        //kBMatrix.Set( iRow, iCol, fValue );
                        kBMatrix.Set( iRow, iCol, fValue );
                        if ( fValue != 0 )
                        {
                            bZero = false;
                        }
                    }
                    m_abBZeroFlag[iRow] = bZero;
                    //System.err.println( "" );
                }
                in.close();
            
                int iBWeight = 0;
                for ( int iRow = 0; iRow < m_iWeights; iRow++ )
                {
                    if ( !m_abBZeroFlag[iRow] )
                    {
                        iBWeight++;
                    }
                }

                m_kBMatrix = new GMatrixf( iBWeight, 6 );
                m_aiB0Index = new int[ m_iWeights-iBWeight ];
                int iMatrixRow = 0;
                int iB0Index = 0;
                for ( int iRow = 0; iRow < m_iWeights; iRow++ )
                {
                    if ( !m_abBZeroFlag[iRow] )
                    {
                        for ( int j = 0; j < 6; j++ )
                        {
                            m_kBMatrix.Set( iMatrixRow, j, kBMatrix.Get(iRow, j ) );
                        }
                        iMatrixRow++;
                    }
                    else
                    {
                        m_aiB0Index[iB0Index++] = iRow;
                    }
                }



                /*
                  int iSeries = 0;
                  for ( int iRow = 0; iRow < m_iWeights-1; iRow++ )
                  {
                  if ( m_abBZeroFlag[iRow] && !m_abBZeroFlag[iRow+1] )
                  {
                  iSeries++;
                  }
                  }
                  m_iSeriesCount = iSeries;
                  m_aaiSeries = new int[m_iSeriesCount][2];
                  for ( int i = 0; i < m_iSeriesCount; i++ )
                  {
                  m_aaiSeries[i][0] = 1;
                  m_aaiSeries[i][1] = 1;
                  }
            
                  iSeries = 0;
                  for ( int iRow = 0; iRow < m_iWeights-1; iRow++ )
                  {
                  if ( m_abBZeroFlag[iRow] && m_abBZeroFlag[iRow+1] )
                  {
                  m_aaiSeries[ iSeries ][0]++;
                  }
                  if ( !m_abBZeroFlag[iRow] && !m_abBZeroFlag[iRow+1] )
                  {
                  m_aaiSeries[ iSeries ][1]++;
                  }
                  if ( !m_abBZeroFlag[iRow] && m_abBZeroFlag[iRow+1] )
                  {
                  iSeries++;
                  }
                  }

                  m_akBMatrix = new GMatrixf[m_iSeriesCount];
                  for ( int i = 0; i < m_iSeriesCount; i++ )
                  {
                  m_akBMatrix[i] = new GMatrixf( m_aaiSeries[i][1], m_aaiSeries[i][1]);
                  System.err.println( m_aaiSeries[i][0] + " " + m_aaiSeries[i][1] );
                  }

                  iSeries = 0;
                  int iMatrixRow = 0;
                  for ( int iRow = 0; iRow < m_iWeights; iRow++ )
                  {
                  if ( !m_abBZeroFlag[iRow] )
                  {
                  for ( int j = 0; j < 6; j++ )
                  {
                  m_akBMatrix[iSeries].Set( iMatrixRow, j, kBMatrix.Get(iRow, j ) );
                  }
                  iMatrixRow++;
                  }
                  if ( ((iRow+1) < m_iWeights) && !m_abBZeroFlag[iRow] && m_abBZeroFlag[iRow+1] )
                  {
                  iSeries++;
                  iMatrixRow = 0;
                  }
                  }

                  for ( int i = 0; i < m_iSeriesCount; i++ )
                  {
                  System.err.println( "Series: " + i );
                  for ( int j = 0; j < m_akBMatrix[i].GetRows(); j++ )
                  {
                  for ( int k = 0; k < 6; k++ )
                  {
                  System.err.print( m_akBMatrix[i].Get(j, k ) + " " );
                  }
                  System.err.println( );
                  }
                  System.err.println( );
                  }
                */
            } catch (IOException e) {}
        }
    }

    /**
     * browses and loads DTI file
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
            kDWIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            m_kTractOutputPath.setText(chooser.getSelectedFile().getAbsolutePath() + "_tract" );

            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());

            m_iSlices = Integer.valueOf(textDim3.getText()).intValue();
            m_iWeights = Integer.valueOf(textDim4.getText()).intValue();
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
                        //System.err.println(str );
                        m_aakDWIList[i][j] = new String(chooser.getSelectedFile().getParentFile() + File.separator + str);
                    }
                }
                in.close();
            } catch (IOException e) {}
        }
    }

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
    }

    private void processDWI()
    {
        //if ( m_akBMatrix == null )
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

        m_iXDim = Integer.valueOf(textDim1.getText()).intValue();
        m_iYDim = Integer.valueOf(textDim2.getText()).intValue();

        int iLen = m_iXDim * m_iYDim * m_iSlices;
        float[] afTensorData = new float[iLen * 6];

        //Matrix kMat = new Matrix( m_aaiSeries[0][1], 6 );
        //for ( int iR = 0; iR < m_aaiSeries[0][1]; iR++ )
        Matrix kMat = new Matrix( m_kBMatrix.GetRows(), 6 );
        for ( int iR = 0; iR < m_kBMatrix.GetRows(); iR++ )
        {
            for ( int iC = 0; iC < 6; iC++ )
            {
                //kMat.set(iR, iC, m_akBMatrix[0].Get(iR,iC));
                kMat.set(iR, iC, m_kBMatrix.Get(iR,iC));
            }
        }

        Matrix kTest1 = new Matrix( 6, 1 );
        for ( int i = 0; i < 6; i++ )
        {
            kTest1.set( i, 0, i );
        }

        Matrix kTest2 = kMat.times( kTest1 );
        System.err.println(  " " );
        System.err.println(  " " );
        System.err.println( "TEST 2" );
        for ( int i = 0; i < kTest2.getRowDimension(); i++ )
        {
            for ( int j = 0; j < kTest2.getColumnDimension(); j++ )
            {
                System.err.print( kTest2.get(i,j ) + " " );
            }
            System.err.println(  " " );
        }

        SingularValueDecomposition kSVD = new SingularValueDecomposition(kMat);

        Matrix kSigma = kSVD.getS();
        for ( int i = 0; i < kSigma.getRowDimension(); i++ )
        {
            for ( int j = 0; j < kSigma.getColumnDimension(); j++ )
            {
                if ( kSigma.get( i, j ) != 0 )
                {
                    kSigma.set( i, j, 1.0f/kSigma.get( i, j ));
                }
            }
        }
        


        Matrix kMatInv = kSVD.getV().times( kSigma.times( kSVD.getU().transpose() ) );

        Matrix kTest3 = kMatInv.times( kTest2 );

        System.err.println(  " " );
        System.err.println(  " " );
        System.err.println( "TEST 3" );
        for ( int i = 0; i < kTest3.getRowDimension(); i++ )
        {
            for ( int j = 0; j < kTest3.getColumnDimension(); j++ )
            {
                System.err.print( kTest3.get(i,j ) + " " );
            }
            System.err.println(  " " );
        }

        //Matrix kMatInv = kSVD.getV().times( kSVD.getU().transpose() );

        Matrix kTest = kMatInv.times(kMat);

        System.err.println( " " );
        System.err.println( " " );
        for ( int i = 0; i < kTest.getRowDimension(); i++ )
        {
            for ( int j = 0; j < kTest.getColumnDimension(); j++ )
            {
                if ( Math.abs(kTest.get( i, j )) < Mathf.ZERO_TOLERANCE )
                {
                    kTest.set( i, j, 0f );
                }
                System.err.print( kTest.get( i, j ) + " " );
            }
            System.err.println( " " );
        }
        System.err.println( " " );

        for ( int iSlice = 0; iSlice < m_iSlices; iSlice++ )
            //int iSlice = m_iSlices/2;
        {
            
            byte[][] aabSliceData = new byte[m_iWeights][];
            for ( int iWeight = 0; iWeight < m_iWeights; iWeight++ )
            {
                File kFile = new File( m_aakDWIList[iSlice][iWeight] );
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
                try {
                    aabSliceData[iWeight] = new byte[iLength];
                    FileInputStream kFileReader = new FileInputStream(kFile);
                    kFileReader.read(aabSliceData[iWeight],0,iLength);
                    kFileReader.close();
                } catch (IOException e ) {}
            }
            for ( int iY = 0; iY < m_iYDim; iY++ )
                //int iY = m_iYDim/2;
            {
                for ( int iX = 0; iX < m_iXDim; iX++ )
                    //int iX = m_iXDim/2;
                {
                    int iIndex = (iY * m_iXDim) + iX;

                    // Read b-zero values:
                    //                     float fS0 = 0;
                    //                     for ( int iB = 0; iB < m_aaiSeries[0][0]; iB++ )
                    //                     {
                    //                         fS0 = readFloat( aabSliceData[ iSliceDataIndex++ ], iIndex );
                    //                     }
                    //                     //fS0 /= (float)m_aaiSeries[0][0];

                    //                     fS0 = (float)Math.log( fS0 );

                    //                     float[] kS = new float[ m_aaiSeries[0][1] ];
                    //                     Matrix kMS = new Matrix(m_aaiSeries[0][1], 1);
                    //                     for ( int iW = 0; iW < m_aaiSeries[0][1]; iW++ )
                    //                     {
                    //                         kS[iW] = fS0 - readFloat( aabSliceData[iSliceDataIndex++], iIndex );
                    //                         //kS[iW] = readFloat( aabSliceData[iSliceDataIndex++], iIndex );
                    //                         kMS.set(iW,0,kS[iW]);
                    //                     }

                    //                     float[] kTensor = new float[ m_aaiSeries[0][1] ];
                    //                     for ( int iT = 0; iT < m_aaiSeries[0][1]; iT++ )
                    //                     {
                    //                         kTensor[iT] = 0f;
                    //                     }

                    float fS0 = 0;
                    int fS0Count = 0;
                    for ( int iW = 0; iW < m_iWeights; iW++ )
                    {
                        if ( m_abBZeroFlag[ iW ] )
                        {
                            float fValue = readFloat( aabSliceData[iW], iIndex );
                            if ( fValue != 0 )
                            {
                                //fValue = (float)Math.log( fValue );
                                fS0 += fValue;
                            }
                            //fS0 += readFloat( aabSliceData[iW], iIndex );
                            fS0Count++;
                        }
                    }
                    //fS0 /= (float)(fS0Count);                   
                    fS0 = (float)Math.log( fS0 );

                    float[] kS = new float[ m_kBMatrix.GetRows() ];
                    Matrix kMS = new Matrix( m_kBMatrix.GetRows(), 1);
                    int iMatrixRow = 0;
                    for ( int iW = 0; iW < m_iWeights; iW++ )
                    {
                        if ( !m_abBZeroFlag[ iW ] )
                        {
                            float fValue = readFloat( aabSliceData[iW], iIndex );
                            if ( fValue != 0 )
                            {
                                fValue = (float)Math.log( fValue );
                            }
                            kS[iMatrixRow] = fS0 - fValue;
                            //kS[iMatrixRow] = fS0 - (float)Math.log(readFloat( aabSliceData[iW], iIndex ) );
                            //kS[iMatrixRow] = readFloat( aabSliceData[iW], iIndex );
                            kMS.set(iMatrixRow,0,kS[iMatrixRow]);
                            iMatrixRow++;
                        }
                    }

                    //Matrix kMT = kMatInv.times(kMS);
                    Matrix kMT = kMS.transpose().times(kMat);
                    //Matrix kMT = kMS.transpose().times(kMatInv.transpose());
                    
                    int index = iSlice * (m_iYDim * m_iXDim) + (iY * m_iXDim) + iX;
                    for ( int iT = 0; iT < 6; iT++ )
                    {
                        //afTensorData[index + iT*iLen] = kTensor[iT];
                        //afTensorData[index + iT*iLen] = (float)kMT.get(iT,0);
                        afTensorData[index + iT*iLen] = (float)kMT.get(0,iT);
                        if ( iT > 2 )
                        {
                            afTensorData[index + iT*iLen] /= 2.0f;
                        }
                    }

                    if ( m_kDTIImage != null )
                    {
                        if ( (iSlice == m_iSlices / 2) )
                        {
                            boolean bAllZero = true;
                            for ( int j = 0; j < 6; j++ )
                            {
                                if ( m_kDTIImage.getFloat( index + j*iLen) !=0 )
                                {
                                    bAllZero = false;
                                }
                            }
                            if ( !bAllZero )
                            {
                                System.err.print( iSlice + " " + iY + " " + iX + " " );
                                for ( int j = 0; j < 6; j++ )
                                {
                                    System.err.print( m_kDTIImage.getFloat( index + j*iLen) + " " +
                                                      afTensorData[index + j*iLen] + " " );
                                }
                                System.err.println("");
                            }
                        }
                    }
                    /*
                      if ( LinearSystem.Solve( m_akBMatrix[0], kS, kTensor ) )
                      {
                      System.err.println( iSlice + " " + iY + " " + iY );

                      int index = iSlice * (m_iYDim * m_iXDim) + (iY * m_iXDim) + iX;
                      for ( int iT = 0; iT < 6; iT++ )
                      {
                      afTensorData[index + iT*iLen] = kTensor[iT];
                      if ( iT > 2 )
                      {
                      afTensorData[index + iT*iLen] /= 2.0f;
                      }
                            
                      if ( iSlice == (m_iSlices/2))
                      {
                      if ( (iY == (m_iYDim/2)) && (iX == (m_iXDim/2)) )
                      {
                      System.err.print(kTensor[iT] + " " );
                      }
                      }
                      }
                      if ( iSlice == (m_iSlices/2))
                      {
                      if ( (iY == (m_iYDim/2)) && (iX == (m_iXDim/2)) )
                      {
                      System.err.println(" " );
                      }
                      }
                      }
                    */
                }
            }
        }

        int[] extents = new int[]{m_iXDim, m_iYDim, m_iSlices, 6};
        ModelImage dtiImage = new ModelImage( ModelStorageBase.FLOAT, extents, new String( "DiffusionTensorImage" ) );
        try {
            dtiImage.importData(0, afTensorData, true );
        } catch (IOException e) {}
        calcEigenVector(dtiImage);

        setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
    }

    private float readFloat( byte[] abData, int iIndex )
    {
        int b1 = abData[iIndex*4 + 0] & 0xff;
        int b2 = abData[iIndex*4 + 1] & 0xff;
        int b3 = abData[iIndex*4 + 2] & 0xff;
        int b4 = abData[iIndex*4 + 3] & 0xff;
        //int tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);
        int tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        float fValue = Float.intBitsToFloat(tmpInt);
        return fValue;
    }


    /**
     * browses and loads DTI file
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
                kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            if(m_kDTIImage.getExtents()[3] != 6) {
                MipavUtil.displayError("Diffusion Tensor does not have correct dimensions");
                if(m_kDTIImage != null) {
                    m_kDTIImage.disposeLocal();
                }
                kDTIPath.setText("");
                m_kDTIImage = null;
                return;
            }
            System.err.println( m_kDTIImage.getExtents()[0] + " " + m_kDTIImage.getExtents()[1] + " " + m_kDTIImage.getExtents()[2] );
            kDTIPath.setText(chooser.getSelectedFile().getAbsolutePath());
            m_kTractOutputPath.setText(chooser.getSelectedFile().getAbsolutePath() + "_tract" );
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());            
        }
    }



    /**
     * browses and loads eigen vector file
     */
    public void loadEigenVectorFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose eigenvector file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            FileIO fileIO = new FileIO();
            if(eigvecSrcImage != null) {
                eigvecSrcImage.disposeLocal();
                eigvecSrcImage = null;
            }
            eigvecSrcImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            if(eigvecSrcImage.getNDims() != 4) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if(eigvecSrcImage != null) {
                    eigvecSrcImage.disposeLocal();
                }
                eigenvectorPath.setText("");
                eigvecSrcImage = null;
                return;
            }
            if(eigvecSrcImage.getExtents()[3] != 9) {
                MipavUtil.displayError("Eigenvector file does not have correct dimensions");
                if(eigvecSrcImage != null) {
                    eigvecSrcImage.disposeLocal();
                }
                eigenvectorPath.setText("");
                eigvecSrcImage = null;
                return;
            }
            eigenvectorPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
            int[] dimExtentsLUT;
            dimExtentsLUT = new int[2];
            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;
            LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
            LUTa.resetTransferLine(0.0f, (int) Math.round(eigvecSrcImage.getMin()), 255.0f, (int) Math.round(eigvecSrcImage.getMax()));
            int[] extents;
            extents = new int[4];
            extents[0] = Math.round(eigvecSrcImage.getExtents()[0]);
            extents[1] = Math.round(eigvecSrcImage.getExtents()[1]);
            extents[2] = Math.round(eigvecSrcImage.getExtents()[2]);
            extents[3] = Math.round(eigvecSrcImage.getExtents()[3]);   
        }
    }
	
	
    /**
     * browses and loads anisotropy file
     */
    public void loadAnisotropyFile() {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose anisotropy file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            FileIO fileIO = new FileIO();
            if(anisotropyImage != null) {
                anisotropyImage.disposeLocal();
                anisotropyImage = null;
            }
            anisotropyImage = fileIO.readImage(chooser.getSelectedFile().getName(),chooser.getCurrentDirectory() + File.separator);
            if(anisotropyImage.getNDims() > 3) {
                MipavUtil.displayError("anisotropy file does not have correct dimensions");
                if(anisotropyImage != null) {
                    anisotropyImage.disposeLocal();
                }
                anisotropyPath.setText("");
                anisotropyImage = null;
                return;
            }
            anisotropyPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }


    private void calcEigenVector( ModelImage dtiImage )
    {
        int iLen = dtiImage.getExtents()[0] * dtiImage.getExtents()[1] * dtiImage.getExtents()[2];
        float[] afData = new float[iLen];
        float[] afDataCM = new float[iLen*9];

        float[] afTensorData = new float[6];
        Vector3f kV1 = new Vector3f( Vector3f.ZERO );
        Vector3f kV2 = new Vector3f( Vector3f.ZERO );
        Vector3f kV3 = new Vector3f( Vector3f.ZERO );

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
        }
    
        int[] extentsEV = new int[]{dtiImage.getExtents()[0], dtiImage.getExtents()[1], dtiImage.getExtents()[2], 9};
        int[] extentsA = new int[]{dtiImage.getExtents()[0], dtiImage.getExtents()[1], dtiImage.getExtents()[2]};


        anisotropyImage = new ModelImage( ModelStorageBase.FLOAT, extentsA, new String( dtiImage.getFileInfo(0).getFileName() + "FA") );
        try {
            anisotropyImage.importData(0, afData, true);
        } catch (IOException e) { }

        eigvecSrcImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, extentsEV, new String( dtiImage.getFileInfo(0).getFileName() + "EG") );
        try {
            eigvecSrcImage.importData(0, afDataCM, true);
        } catch (IOException e) { }

        //Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        int[] dimExtentsLUT;
        dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        LUTa = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
        LUTa.resetTransferLine(0.0f, (int) Math.round(eigvecSrcImage.getMin()), 255.0f, (int) Math.round(eigvecSrcImage.getMax()));
        int[] extents;
        extents = new int[4];
        extents[0] = Math.round(eigvecSrcImage.getExtents()[0]);
        extents[1] = Math.round(eigvecSrcImage.getExtents()[1]);
        extents[2] = Math.round(eigvecSrcImage.getExtents()[2]);
        extents[3] = Math.round(eigvecSrcImage.getExtents()[3]);   

        if ( m_kReconstructTracts.isSelected() )
        {
            reconstructTracts( dtiImage, eigvecSrcImage );
        }



    }

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
        kDTIPath = new JTextField(35);
        kDTIPath.setEditable(false);
        kDTIPath.setBackground(Color.white);
        kDTIFilesPanel.add(kDTIPath, gbc);
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
        eigenvectorPath = new JTextField(35);
        eigenvectorPath.setEditable(false);
        eigenvectorPath.setBackground(Color.white);
        filesPanel.add(eigenvectorPath, gbc);
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
        anisotropyPath = new JTextField(35);
        anisotropyPath.setEditable(false);
        anisotropyPath.setBackground(Color.white);
        filesPanel.add(anisotropyPath, gbc);
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
        kTractsLimit = new JTextField("100", 5);
        kTractsLimit.setBackground(Color.white);
        kParamsPanel.add(kTractsLimit, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kTractsMinLength = new JLabel( "Minimum tract length:" );
        kParamsPanel.add(kTractsMinLength, gbc);
        gbc.gridx++;
        kTractsMin = new JTextField("50", 5 );
        kTractsMin.setBackground(Color.white);
        kParamsPanel.add(kTractsMin, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kTractsMaxLength = new JLabel( "Maximum tract length:" );
        kParamsPanel.add(kTractsMaxLength, gbc);
        gbc.gridx++;
        kTractsMax = new JTextField("100", 5 );
        kTractsMax.setBackground(Color.white);
        kParamsPanel.add(kTractsMax, gbc);

        gbc.gridx = 0;
        gbc.gridy++;
        JLabel kUseVOI = new JLabel( "Use VOI:" );
        kParamsPanel.add(kUseVOI, gbc);
        gbc.gridx++;
        kUseVOICheck = new JCheckBox("use voi" );
        kParamsPanel.add(kUseVOICheck, gbc);

        
        JPanel filesPanel = new JPanel(kGBL);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,5,5);
        JLabel kTractLabel = new JLabel(" DTI tract file: ");
        filesPanel.add(kTractLabel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        kTractPath = new JTextField(35);
        kTractPath.setEditable(false);
        kTractPath.setBackground(Color.white);
        filesPanel.add(kTractPath, gbc);
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

        textDim1 = new JTextField(5);
        textDim1.setText("128");
        textDim1.setFont(serif12);
        textDim1.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 0;
        panelDims.add(textDim1, gbcPanelDims);

        textRes1 = new JTextField(5);
        textRes1.setText("1.0");
        textRes1.setFont(serif12);
        textRes1.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 0;
        panelDims.add(textRes1, gbcPanelDims);

        dim2 = new JLabel("2nd");
        dim2.setFont(serif12);
        dim2.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 1;
        panelDims.add(dim2, gbcPanelDims);

        textDim2 = new JTextField(5);
        textDim2.setText("157");
        textDim2.setFont(serif12);
        textDim2.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 1;
        panelDims.add(textDim2, gbcPanelDims);

        textRes2 = new JTextField(5);
        textRes2.setText("1.0");
        textRes2.setFont(serif12);
        textRes2.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 1;
        panelDims.add(textRes2, gbcPanelDims);

        dim3 = new JLabel("3rd");
        dim3.setFont(serif12);
        dim3.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 2;
        panelDims.add(dim3, gbcPanelDims);

        textDim3 = new JTextField(5);
        textDim3.setText("114");
        textDim3.setFont(serif12);
        textDim3.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 2;
        panelDims.add(textDim3, gbcPanelDims);

        textRes3 = new JTextField(5);
        textRes3.setText("1.0");
        textRes3.setFont(serif12);
        textRes3.addFocusListener(this);
        gbcPanelDims.gridx = 2;
        gbcPanelDims.gridy = 2;
        panelDims.add(textRes3, gbcPanelDims);

        dim4 = new JLabel("4th");
        dim4.setFont(serif12);
        dim4.setForeground(Color.black);
        gbcPanelDims.gridx = 0;
        gbcPanelDims.gridy = 3;
        panelDims.add(dim4, gbcPanelDims);

        textDim4 = new JTextField(5);
        textDim4.setText("72");
        textDim4.setFont(serif12);
        textDim4.addFocusListener(this);
        gbcPanelDims.gridx = 1;
        gbcPanelDims.gridy = 3;
        panelDims.add(textDim4, gbcPanelDims);

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
        kBMatrixPath = new JTextField(35);
        kBMatrixPath.setEditable(false);
        kBMatrixPath.setBackground(Color.white);
        kDWIFilesPanel.add(kBMatrixPath, gbc);
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
        kDWIPath = new JTextField(35);
        kDWIPath.setEditable(false);
        kDWIPath.setBackground(Color.white);
        kDWIFilesPanel.add(kDWIPath, gbc);
        gbc.gridx = 2;
        JButton kDWIBrowseButton = new JButton("Browse");
        kDWIBrowseButton.addActionListener(this);
        kDWIBrowseButton.setActionCommand("DWIBrowse");
        kDWIFilesPanel.add(kDWIBrowseButton, gbc);
        
        JPanel kFileInstructions = new JPanel();
        kFileInstructions.setLayout(new GridLayout( 1, 1 ));
        kFileInstructions.setBorder(buildTitledBorder("2). Select the BMatrix and .path files"));
        kFileInstructions.add(kDWIFilesPanel);
        
        kDWIPanel.add(kDimsInstructions);
        kDWIPanel.add(kFileInstructions);
        return kDWIPanel;
    }

    private void reconstructTracts( ModelImage dtiImage, ModelImage eigenImage )
    {
        m_kDTIImage = dtiImage;
        m_kEigenImage = eigenImage;
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
                        afVectorData[j] = m_kEigenImage.getFloat(i + j*iLen);
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
        }
        System.err.println( "Number of tracts: " + iCount + " tract size " + iTractSize );

        try {
            kFileWriter.close();
        } catch ( IOException e2 ) {}
    }

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

    private boolean contains( ModelImage kMask, Vector<Integer> kTract )
    {
        if ( kMask == null )
        {
            return true;
        }
        for ( int i = 0; i < kTract.size(); i++ )
        {
            int iIndex = kTract.get(i);
            if ( kMask.getBoolean(iIndex) )
            {
                return true;
            }
        }
        return false;
    }


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

    private void loadTractFile()
    {
        JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.ALL));
        chooser.setDialogTitle("Choose Diffusion Tensor Tract file");
        int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) { 	
            FileInputStream kFileReader;
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
            kTractPath.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());            
        }
    }

    private void processTractFile()
    {
        if ( m_kTractFile == null )
        {
            MipavUtil.displayError("Tract file must be set.");
            return;
        }

        try {
            int iNumTractsLimit = (new Integer( kTractsLimit.getText() )).intValue() ;
            int iTractMinLength = (new Integer( kTractsMin.getText() )).intValue() ;
            int iTractMaxLength = (new Integer( kTractsMax.getText() )).intValue() ;

            int iNumTracts = 0;

            ModelImage kMask = null;
            if ( kUseVOICheck.isSelected() )
            {
                kMask = ViewUserInterface.getReference().getActiveImageFrame().getActiveImage().generateBinaryImage(false, true);
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
                if ( contains( kMask, kTract ) )
                {
                    if ( (iVQuantity > iTractMinLength) && (iVQuantity < iTractMaxLength) )
                    {
                        if ( iNumTracts < iNumTractsLimit )
                        {
                            iNumTracts++;
                            System.err.println( "Loading tract: size = " + iVQuantity);
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


    private boolean subset( Vector<Integer> kTract1, Vector<Integer> kTract2,
                            boolean bInvert  )
    {
        Vector<Integer> kV1 = kTract1;
        if ( bInvert )
        {
            kV1 = new Vector<Integer>();
            int iVQuantity1 = kTract1.size();
            for ( int i = 0; i < iVQuantity1; i++ )
            {
                kV1.add( 0, kTract1.get(i) );
            }
        }

        int iVQuantity1 = kV1.size();
        int iVQuantity2 = kTract2.size();

        if ( iVQuantity1 == iVQuantity2 )
        {
            for ( int i = 0; i < iVQuantity1; i++ )
            {
                if ( !(kV1.get(i).intValue() == kTract2.get(i).intValue()) )
                {
                    return false;
                }
            }
            return true;
        }

        for ( int i = 0; i < iVQuantity2; i++ )
        {
            if ( (kV1.get(0).intValue() == kTract2.get(i).intValue()) )
            {
                boolean bEqual = true;
                for ( int j = 0; j < iVQuantity1; j++ )
                {
                    if ( (i + j) >= iVQuantity2 )
                    {
                        return false;
                    }
                    if ( !(kV1.get(j).intValue() == kTract2.get(i+j).intValue()) )
                    {
                        bEqual = false;
                    }
                }
                if ( bEqual )
                {
                    return true;
                }
            }
        }
        return false;
    }
};
