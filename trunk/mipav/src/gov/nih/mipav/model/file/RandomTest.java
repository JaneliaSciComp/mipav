package gov.nih.mipav.model.file;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

public class RandomTest {

    /**
     * @param args
     */
    public static void main(String[] args) {
        try {
            File f = new File(args[0]);
            if(f.exists()) {
                System.out.println();
            }
            RandomAccessFile rf = new RandomAccessFile(f, "r");
            byte[] b;
            try {
                b = new byte[(int) rf.length()];
                rf.readFully(b);
            } catch (IOException e1) {
                // TODO Auto-generated catch block
                e1.printStackTrace();
            }
        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

}
