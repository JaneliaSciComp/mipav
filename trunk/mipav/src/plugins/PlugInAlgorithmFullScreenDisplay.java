
import java.awt.Component;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.GraphicsDevice;
import java.awt.GraphicsEnvironment;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;

import javax.swing.JTextArea;

import gov.nih.mipav.model.algorithms.AlgorithmBase;



public class PlugInAlgorithmFullScreenDisplay extends AlgorithmBase {
	private BufferedImage inputImage;
	private BufferedImage cornerImage;
	
	private JTextArea outputTextArea;


	public PlugInAlgorithmFullScreenDisplay(BufferedImage inputImage, BufferedImage cornerImage, JTextArea outputTextArea) {
		this.inputImage = inputImage;
		this.cornerImage = cornerImage;
		this.outputTextArea = outputTextArea;
		
	}
	
	
	public void runAlgorithm() {
		outputTextArea.append("Running Algorithm v1.0" + "\n");
        
        final long begTime = System.currentTimeMillis();
        final int inputWidth = inputImage.getWidth();
        final int inputHeight = inputImage.getHeight();
        Frame frame = new Frame("Test");
        frame.setUndecorated(true);
        frame.add(new Component() {
        	public void paint(Graphics g) {
        		super.paint(g);
        		double widthRatio = (double)(getWidth()-160)/(double)inputWidth;
        		double heightRatio = (double)(getHeight()-158)/(double)inputHeight;
        		BufferedImage backgroundImage = new BufferedImage(getWidth(), getHeight(),BufferedImage.TYPE_INT_ARGB);
        		int length = getWidth() * getHeight();
        		int[] bufferData = new int[length*4];
    			for (int i = 0; i < length; i++)
    			{
    				bufferData[i*4 + 0] = 0;
    				bufferData[i*4 + 1] = 0;
    				bufferData[i*4 + 2] = 0;
    				bufferData[i*4 + 3] = 255;
    			}
    			backgroundImage.getRaster().setPixels(0,0, getWidth(), getHeight(), bufferData );
    			g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
        		if (widthRatio > heightRatio) {
        			// Can only expand by the heightRatio
        			int expWidth = (int)Math.floor(inputWidth*heightRatio);
        			int leftPadding = (getWidth() - 160 - expWidth)/2;
        		    g.drawImage(inputImage, 80 + leftPadding, 79, expWidth, getHeight()-158, this);
        		}
        		else {
        			// Can only expand by the widthRatio
        			int expHeight = (int)Math.floor(inputHeight*widthRatio);
        			int topPadding = (getHeight() - 158 - expHeight)/2;
        			g.drawImage(inputImage, 80, 79 + topPadding, getWidth() - 160, expHeight, this);
        		}
        		if (cornerImage != null) {
        		    g.drawImage(cornerImage, 0, 0, 80, 79, this);
        		    g.drawImage(cornerImage, getWidth() - 80, 0, 80, 79, this);
        		    g.drawImage(cornerImage, 0, getHeight() - 79, 80, 79, this);
        		    g.drawImage(cornerImage, getWidth() - 80, getHeight() - 70, 80, 79, this);
        		} // if (cornerImage != null)
        	}
        });
        frame.addMouseListener(new MouseListener() {
            public void mouseClicked (MouseEvent e) {
            	System.exit(0);
            }
            public void mousePressed(MouseEvent e){}
            public void mouseReleased(MouseEvent e){}
            public void mouseEntered(MouseEvent e){}
            public void mouseExited(MouseEvent e){}
        });
        GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        GraphicsDevice gs = ge.getDefaultScreenDevice();
        gs.setFullScreenWindow(frame);
        frame.validate();
		
		final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

	}
	
	
	

}
