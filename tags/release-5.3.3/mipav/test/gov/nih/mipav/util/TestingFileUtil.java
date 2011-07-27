package gov.nih.mipav.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.RandomAccessFile;

import WildMagic.LibFoundation.Mathematics.GMatrixf;

public class TestingFileUtil {
    /**
     * Read the DTI path file.
     * @param pathFileName
     * @param nslices
     * @param nweights
     * @return
     */
    public static String[][] readPathFile(String pathFileName, int nslices, int nweights) {
        File kFile = new File(pathFileName);
        if (!kFile.exists() || !kFile.canRead()) {
            return null;
        }
        int iLength = (int) kFile.length();
        if (iLength <= 0) {
            return null;
        }
        String[][] dwiFileNameList = new String[nslices][nweights];
        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            for (int i = 0; i < nslices; i++) {
                for (int j = 0; j < nweights; j++) {
                    str = in.readLine();
                    dwiFileNameList[i][j] = new String(str);
                }
            }
            in.close();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
        
        return dwiFileNameList;
    }
    
    /**
     * Read DTI B Matrix File.
     * @param bMatrixFileName
     * @param nweights
     * @param matrixEntries
     * @return
     */
    public static GMatrixf readDTIBMatrixFile( String bMatrixFileName, int nweights, int[] matrixEntries) {
        File kFile = new File(bMatrixFileName);
        if ( !kFile.exists() || !kFile.canRead() )
        {
            return null;
        }
        int iLength = (int)kFile.length();
        if ( iLength <= 0 )
        {
            return null;
        }

        try {
            BufferedReader in = new BufferedReader(new FileReader(kFile));
            String str;
            
            GMatrixf bMatrix = new GMatrixf( nweights, 6 + 1 );
            
            String[] kBMatrixString = new String[nweights];
            int nb = 0;
            
            matrixEntries = new int[nweights];
            for ( int iRow = 0; iRow < nweights; iRow++ )
            {
                str = in.readLine();

                boolean gotit = false;
                for (int j=0; j < nb ; j++) { 
                    if (str.equals(kBMatrixString[j]))
                    {
                        gotit=true;
                        matrixEntries[iRow]=j;
                        break;
                    }
                }
                if (!gotit)
                { 
                    kBMatrixString[nb]=str;
                    matrixEntries[iRow]=nb;
                    nb=nb+1;
                }   

                java.util.StringTokenizer st = new java.util.StringTokenizer(str);
                for ( int iCol = 0; iCol < 6; iCol++ )
                {
                    float fValue = Float.valueOf(st.nextToken()).floatValue();
                    bMatrix.Set( iRow, iCol, fValue );
                }
                bMatrix.Set( iRow, 6, 1f );
            } 
            in.close();
            return bMatrix;

        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    
    /**
     * Read a short from a file.
     * @param raFile
     * @param bigEndian
     * @return
     * @throws IOException
     */
    public static short readShort(RandomAccessFile raFile, boolean bigEndian) throws IOException {
        short tempShort = 0;
        byte[] buffer = new byte[2];

        raFile.readFully(buffer);

        if (bigEndian) {
            tempShort = (short) (((buffer[0] & 0xff) << 8) | (buffer[1] & 0xff));
        } else {
            tempShort = (short) (((buffer[1] & 0xff) << 8) | (buffer[0] & 0xff));
        }

        return tempShort;
    }

    /**
     * Read short data from a file.
     * @param filename
     * @param bigEndian
     * @return
     */
    public static short[] readRawFileShort(String filename, boolean bigEndian){
        RandomAccessFile raFile = null;
        try{
            raFile = new RandomAccessFile(new File(filename), "r");
            int length = (int)(raFile.length()/2);
            short[] data = new short[length];
            for(int i = 0; i < length; i++){
                data[i] = readShort(raFile, bigEndian);
            }
            return data;
        }catch(FileNotFoundException e){
            e.printStackTrace();
            return null;
        }catch(IOException e){
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Read a float from a file.
     * @param raFile
     * @param bigEndian
     * @return
     * @throws IOException
     */
    public static float readFloat(RandomAccessFile raFile, boolean bigEndian) throws IOException {
        byte[] buffer = new byte[4];
        raFile.readFully(buffer);

        int tmpInt;

        if (bigEndian) {
            tmpInt = (((buffer[0] & 0xff) << 24) | ((buffer[1] & 0xff) << 16) | ((buffer[2] & 0xff) << 8) |
                          (buffer[3] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = (((buffer[3] & 0xff) << 24) | ((buffer[2] & 0xff) << 16) | ((buffer[1] & 0xff) << 8) |
                          (buffer[0] & 0xff));

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Read float data from a file.
     * @param filename
     * @param bigEndian
     * @return
     */
    public static float[] readRawFileFloat(String filename, boolean bigEndian){
        RandomAccessFile raFile = null;
        try{
            raFile = new RandomAccessFile(new File(filename), "r");
            int length = (int)(raFile.length()/4);
            float[] data = new float[length];
            for(int i = 0; i < length; i++){
                data[i] = readFloat(raFile, bigEndian);
            }
            return data;
        }catch(FileNotFoundException e){
            e.printStackTrace();
            return null;
        }catch(IOException e){
            e.printStackTrace();
            return null;
        }
    }
    
    /**
     * Write float data to a file.
     * @param raFile
     * @param data
     * @param bigEndian
     * @throws IOException
     */
    public static void writeFloat(RandomAccessFile raFile, float data, boolean bigEndian) throws IOException {
        int idata = Float.floatToIntBits(data);
        byte[] buffer = new byte[4];

        if (bigEndian) {
            buffer[0] = (byte) (idata >>> 24);
            buffer[1] = (byte) (idata >>> 16);
            buffer[2] = (byte) (idata >>> 8);
            buffer[3] = (byte) (idata & 0xff);
        } else {
            buffer[0] = (byte) (idata & 0xff);
            buffer[1] = (byte) (idata >>> 8);
            buffer[2] = (byte) (idata >>> 16);
            buffer[3] = (byte) (idata >>> 24);
        }

        raFile.write(buffer);
    }

    /**
     * Write a float to a file.
     * @param filename      the file name
     * @param bigEndian     
     * @param data          float data
     */
    public static void writeRawFileFloat(String filename, boolean bigEndian, float[] data){
        RandomAccessFile raFile = null;
        try{
            raFile = new RandomAccessFile(new File(filename), "rw");
            int length = data.length;
            for(int i = 0; i < length; i++){
                writeFloat(raFile, data[i], bigEndian);
            }
            raFile.close();
        }catch(FileNotFoundException e){
            e.printStackTrace();
        }catch(IOException e){
            e.printStackTrace();
        }
        return;
    }

}
