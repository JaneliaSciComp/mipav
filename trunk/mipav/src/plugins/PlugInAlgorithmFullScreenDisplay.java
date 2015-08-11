import gov.nih.mipav.model.algorithms.AlgorithmBase;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;

import javax.swing.JTextArea;


public class PlugInAlgorithmFullScreenDisplay extends AlgorithmBase {
    private final BufferedImage inputImage;

    private final Image cornerImage;

    private final JTextArea outputTextArea;

    public PlugInAlgorithmFullScreenDisplay(final BufferedImage inputImage, final Image cornerImage, final JTextArea outputTextArea) {
        this.inputImage = inputImage;
        this.cornerImage = cornerImage;
        this.outputTextArea = outputTextArea;

    }

    @Override
    public void runAlgorithm() {
        outputTextArea.append("Running Algorithm v1.0" + "\n");

        final long begTime = System.currentTimeMillis();
        final int inputWidth = inputImage.getWidth();
        final int inputHeight = inputImage.getHeight();
        final Frame frame = new Frame("Test");
        frame.setUndecorated(true);
        frame.add(new Component() {
            @Override
            public void paint(final Graphics g) {
                super.paint(g);
                final double widthRatio = (double) (getWidth() - 160) / (double) inputWidth;
                final double heightRatio = (double) (getHeight() - 158) / (double) inputHeight;
                final BufferedImage backgroundImage = new BufferedImage(getWidth(), getHeight(), BufferedImage.TYPE_INT_ARGB);
                final int length = getWidth() * getHeight();
                final int[] bufferData = new int[length * 4];
                for (int i = 0; i < length; i++) {
                    bufferData[i * 4 + 0] = 0;
                    bufferData[i * 4 + 1] = 0;
                    bufferData[i * 4 + 2] = 0;
                    bufferData[i * 4 + 3] = 255;
                }
                backgroundImage.getRaster().setPixels(0, 0, getWidth(), getHeight(), bufferData);
                g.drawImage(backgroundImage, 0, 0, getWidth(), getHeight(), this);
                if (widthRatio > heightRatio) {
                    // Can only expand by the heightRatio
                    final int expWidth = (int) Math.floor(inputWidth * heightRatio);
                    final int leftPadding = (getWidth() - 160 - expWidth) / 2;
                    g.drawImage(inputImage, 80 + leftPadding, 79, expWidth, getHeight() - 158, this);
                } else {
                    // Can only expand by the widthRatio
                    final int expHeight = (int) Math.floor(inputHeight * widthRatio);
                    final int topPadding = (getHeight() - 158 - expHeight) / 2;
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
            @Override
            public void mouseClicked(final MouseEvent e) {
                System.exit(0);
            }

            @Override
            public void mousePressed(final MouseEvent e) {}

            @Override
            public void mouseReleased(final MouseEvent e) {}

            @Override
            public void mouseEntered(final MouseEvent e) {}

            @Override
            public void mouseExited(final MouseEvent e) {}
        });
        final GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
        final GraphicsDevice gs = ge.getDefaultScreenDevice();
        gs.setFullScreenWindow(frame);
        frame.validate();

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);

    }

}
