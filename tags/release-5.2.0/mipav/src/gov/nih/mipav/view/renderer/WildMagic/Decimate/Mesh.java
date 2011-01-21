package gov.nih.mipav.view.renderer.WildMagic.Decimate;
import java.io.*;

import javax.swing.JFileChooser;
import WildMagic.LibFoundation.Mathematics.*;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewUserInterface;

public class Mesh {

	int numberV = 0;
	int numberF = 0;
	public Vector3f[] verts;
	double scaler = 1.0;
	Vector3f ddnormalv3d = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f v1v3d = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f v2v3d = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f tmppoint3d1 = new Vector3f(0.0f, 0.0f, 0.0f);
	Vector3f tmppoint3d2 = new Vector3f(0.0f, 0.0f, 0.0f);

	boolean pastelnormal = true;
	int type = 0;

	public Mesh(int id) {
		type = id;
	}

	/**
     * Dispose the local memory.
     */
	public void dispose() {
		for ( int i = 0; i < numberF * 3; i++ ) {
			verts[i] = null;
		}
		verts = null;
		ddnormalv3d = null;
		v1v3d = null;
		v2v3d = null;
		tmppoint3d1 = null;
		tmppoint3d2 = null;
	}
	
	public void reNew(int dv, int df, double dcaler) {
		numberV = dv;
		numberF = df;
		scaler = dcaler;
		MemoryAllocation();
	}
	

	private void MemoryAllocation() {
		verts = new Vector3f[(numberF * 3)];
		int i = 0;
		int j = 0;
		for (i = 0; i < numberF; i++) {
			for (j = 0; j < 3; j++) {
				verts[(3 * i + j)] = new Vector3f(0.0f, 0.0f, 0.0f);
			}
		}
	}

	

	/**
	 * Sate the STIL ACII file format. 
	 * @param kName  file name
	 * @throws IOException
	 */
	public void saveAsSTLAsciiFile(int numF, int[][] face, Vector3f[] point, ModelImage imageA)
			throws IOException {
		String kName = getFileName(false);
		PrintWriter kOut = new PrintWriter(new FileWriter(kName));
		printSTLAscii(kOut, numF, face, point, imageA);
		kOut.close();
	}

	protected void printSTLAscii(PrintWriter kOut, int numF, int[][] face,
			Vector3f[] point, ModelImage kImage) throws IOException {
		int i;
		double x, y, z;
		kOut.println("solid");
		int j = 0;
		
		  float[] startLocation = kImage.getFileInfo(0).getOrigin();
          float[] resolution = kImage.getFileInfo(0).getResolutions();
          int[] extents = kImage.getExtents();
          int xDim = extents[0];
          int yDim = extents[1];
          int zDim = extents[2];
          
          float[] resols = kImage.getFileInfo()[0].getResolutions();
          float xBox = (xDim - 1) * resols[0];
          float yBox = (yDim - 1) * resols[1];
          float zBox = (zDim - 1) * resols[2];
          float maxBox = Math.max(xBox, Math.max(yBox, zBox));

          int[] direction = MipavCoordinateSystems.getModelDirections(kImage);
          float[] box = new float[]{ xBox, yBox, zBox };
          
		
		for (i = 0; i < numF; i++) {
			int[] Face = new int[3];
			Face[0] = face[i][0];
			Face[1] = face[i][1];
			Face[2] = face[i][2];
			v1v3d.X = (float)(((point[Face[0]]).X - (point[Face[1]]).X) * scaler);
			v1v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[1]]).Y) * scaler);
			v1v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[1]]).Z) * scaler);
			v2v3d.X = (float)(((point[Face[0]]).X - (point[Face[2]]).X) * scaler);
			v2v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[2]]).Y) * scaler);
			v2v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[2]]).Z) * scaler);

			ddnormalv3d.Cross( v1v3d, v2v3d );
			ddnormalv3d.Normalize();

			kOut.print(" facet normal  ");
			kOut.print(ddnormalv3d.X);
			kOut.print(' ');
			kOut.print(ddnormalv3d.Y);
			kOut.print(' ');
			kOut.println(ddnormalv3d.Z);

			kOut.println("   outer loop");

			
			
			for (j = 0; j < 3; j++) {
				x = point[Face[j]].X * scaler;
				y = point[Face[j]].Y * scaler;
				z = point[Face[j]].Z * scaler;

				
                x = ((((x * 2.0f * maxBox) + xBox) / 2.0f) * direction[0]) + startLocation[0];
                y = ((((y * 2.0f * maxBox) + yBox) / 2.0f) * direction[1]) + startLocation[1];
                z = ((((z * 2.0f * maxBox) + zBox) / 2.0f) * direction[2]) + startLocation[2];
				
				kOut.print("     vertex ");
				kOut.print(x);
				kOut.print(' ');
				kOut.print(y);
				kOut.print(' ');
				kOut.println(z);

			}
			kOut.println("   endloop");
			kOut.println(" endfacet");
		}
		kOut.println("endsolid");
	}

	public void saveAsSTLBinaryFile(int numF, int[][] face, Vector3f[] point)
			throws IOException {
		String kName = getFileName(false);
		RandomAccessFile kOut = new RandomAccessFile(new File(kName), "rw");
		printSTLBinary(kOut, numF, face, point);
		kOut.close();
	}
	
	 /**
     * Returns an array of File objects, based on the user-selected files from the FileChooser dialog.
     *
     * @param   bLoad  whether the files are opened for reading (bLoad = true) or writing (bLoad = false)
     *
     * @return  File[] array of opened files.
     */
    private static File[] openFiles(boolean bLoad) {

        // file dialog to select surface mesh files (*.sur)
        JFileChooser chooser = new JFileChooser();
        chooser.setMultiSelectionEnabled(bLoad);
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

        if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
        } else {
            chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
        }

        int returnVal;

        if (bLoad) {
            returnVal = chooser.showOpenDialog(null);
        } else {
            returnVal = chooser.showSaveDialog(null);
        }

        if (returnVal == JFileChooser.APPROVE_OPTION) {
            ViewUserInterface.getReference().setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) +
                                                                 File.separatorChar);

            if (bLoad) {
                File[] files = chooser.getSelectedFiles();

                return files;
            } else {
                File[] files = new File[1];
                files[0] = chooser.getSelectedFile();

                return files;
            }
        }

        return null;
    }

	
	/**
     * Calls a dialog to get a file name.
     *
     * @param   bLoad  if <code>true</code>, make it a load dialog.
     *
     * @return  File name.
     */
    private static String getFileName(boolean bLoad) {
        File[] files = openFiles(bLoad);

        if (files != null) {
            return new String(files[0].getPath());
        }

        return null;
    }
	
	protected void printSTLBinary(RandomAccessFile kOut, int numF,
			int[][] face, Vector3f[] point) throws IOException {
		int i;

		byte[] attribute = new byte[2];
		int iTriangleCount = numberF;

		// nothing header
		byte[] header = new byte[80];
		kOut.write(header);

		// number of facets
		byte[] buff = new byte[4];
		kOut.write(FileBase.intToBytes(iTriangleCount, false, buff));
		byte[] buff2 = new byte[4];
		int j = 0;
		for (i = 0; i < numF; i++) {
			int[] Face = new int[3];
			Face[0] = face[i][0];
			Face[1] = face[i][1];
			Face[2] = face[i][2];
			v1v3d.X = (float)(((point[Face[0]]).X - (point[Face[1]]).X) * scaler);
			v1v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[1]]).Y) * scaler);
			v1v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[1]]).Z) * scaler);
			v2v3d.X = (float)(((point[Face[0]]).X - (point[Face[2]]).X) * scaler);
			v2v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[2]]).Y) * scaler);
			v2v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[2]]).Z) * scaler);
			ddnormalv3d.Cross( v1v3d, v2v3d );
			ddnormalv3d.Normalize();

			kOut.write(FileBase.floatToBytes((float) ddnormalv3d.X, false,buff2));
			kOut.write(FileBase.floatToBytes((float) ddnormalv3d.Y, false,buff2));
			kOut.write(FileBase.floatToBytes((float) ddnormalv3d.Z, false,buff2));

			for (j = 0; j < 3; j++) {
				verts[(3 * i + j)].X = (float)(point[Face[j]].X * scaler);
				verts[(3 * i + j)].Y = (float)(point[Face[j]].Y * scaler);
				verts[(3 * i + j)].Z = (float)(point[Face[j]].Z * scaler);

				kOut.write(FileBase.floatToBytes((float) verts[(3 * i + j)].X,
						false,buff2));
				kOut.write(FileBase.floatToBytes((float) verts[(3 * i + j)].Y,
						false,buff2));
				kOut.write(FileBase.floatToBytes((float) verts[(3 * i + j)].Z,
						false,buff2));

			}

			// 2 byte attribute == 0
			kOut.write(attribute);

		}
		// kOut.close();
	}

	public void MakeFace(int i, int di, int dj, int dk, Vector3f[] point) {
		int[] Face = new int[3];
		Face[0] = di;
		Face[1] = dj;
		Face[2] = dk;
		v1v3d.X = (float)(((point[Face[0]]).X - (point[Face[1]]).X) * scaler);
		v1v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[1]]).Y) * scaler);
		v1v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[1]]).Z) * scaler);
		v2v3d.X = (float)(((point[Face[0]]).X - (point[Face[2]]).X) * scaler);
		v2v3d.Y = (float)(((point[Face[0]]).Y - (point[Face[2]]).Y) * scaler);
		v2v3d.Z = (float)(((point[Face[0]]).Z - (point[Face[2]]).Z) * scaler);
		ddnormalv3d.Cross( v1v3d, v2v3d );
		ddnormalv3d.Normalize();
		int j = 0;

		for (j = 0; j < 3; j++) {
			verts[(3 * i + j)].X = (float)(point[Face[j]].X * scaler);
			verts[(3 * i + j)].Y = (float)(point[Face[j]].Y * scaler);
			verts[(3 * i + j)].Z = (float)(point[Face[j]].Z * scaler);
		}

	}

}
