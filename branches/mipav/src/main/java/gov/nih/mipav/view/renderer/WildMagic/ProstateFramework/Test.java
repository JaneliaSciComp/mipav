package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import java.io.File;

public class Test {
	public static void main(String[] args) {
		String dir = "/scratch/aam_test/fold2/image49.xml";
		int index1 = dir.lastIndexOf(File.separator);
		int index2 = dir.lastIndexOf(".");
		
		String fileName = new String(dir.substring(index1 + 1, index2));
		String numString = fileName.substring(5, fileName.length());
		System.err.println("numString = " + numString);
		int imageNumber = Integer.valueOf(numString);
		System.err.println("imageNumber = " + imageNumber);
	}
}

