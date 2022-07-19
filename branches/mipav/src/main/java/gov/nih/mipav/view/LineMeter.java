package gov.nih.mipav.view;


import java.awt.*;

import javax.swing.*;


/**
 * LineMeter is a history-graph panel which updates its display once a second, drawing a curve of the history of inputs
 * to setAmplitude(). It resizes to preserve the proportions given in the graph, and will record enough data to display
 * across the entire screen.
 *
 * <p>there are a few artifacts in the drawing routine, namely, that setting the update rate to smaller than the
 * animation rate will cause the background graph to update oddly.</p>
 */
public class LineMeter extends JPanel implements Runnable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7794163419733734911L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int animationFrame; // the current repaint.  normally zero, so system repaint does not affect scrolling

    /** monitor animation happens in its own thread. */
    private volatile Thread animationThread;

    /** DOCUMENT ME! */
    private int aniSampleRate = 1000; // animation sample-rate (1s)

    /** DOCUMENT ME! */
    private int chartWidth, chartHeight;

    /** DOCUMENT ME! */
    private int chartXend, chartYend;

    /** DOCUMENT ME! */
    private int chartXstart, chartYstart;

    /** DOCUMENT ME! */
    private boolean counterLock = false; // will not accept any updates during the update buffer copy.
                                         // All updates are dropped when true

    /** DOCUMENT ME! */
    private int countSinceLastUpdate = 0; // number of buffered updates since the most recent animation update

    /** DOCUMENT ME! */
    private Dimension dim;

    /** DOCUMENT ME! */
    private int drawingWidth, drawingHeight;

    /** DOCUMENT ME! */
    private Graphics2D g2d;

    /** DOCUMENT ME! */
    private Insets insets;

    /** DOCUMENT ME! */
    private Color lineColor = Color.darkGray; // new Color(160,60,60);//Color(144, 0, 0);

    /** DOCUMENT ME! */
    private int lineSpacing;

    /** DOCUMENT ME! */
    private int numberOfDivisions;

    /** DOCUMENT ME! */
    private int numberOfSamplesDisplayed; // number of samples taken and shown on the graph

    /** DOCUMENT ME! */
    private int numberOfSamplesPerDivision;

    /** DOCUMENT ME! */
    private int panelWidth = 0, panelHeight = 0;

    /** DOCUMENT ME! */
    private Color paperColor = Color.black;

    /** DOCUMENT ME! */
    private Color penColor = Color.cyan; // Color.red;

    /** DOCUMENT ME! */
    //private int pixelsPerDivision;

    /** DOCUMENT ME! */
    private int pixelsWidePerSample = 5; // number of pixels between each sample added to the history buffer

    /** DOCUMENT ME! */
    private float[] recentSamples; // y-locations not yet in history & not since last monitor refresh

    /** DOCUMENT ME! */
    private int sampleRate; // updates to the number of points can be
                            // expected at a rate of 1 per this many milliseconds

    /** DOCUMENT ME! */
    private int samplesPerUpdate = 1; // integer number of samples

    /** DOCUMENT ME! */
    private int whitespaceH = 10;

    /** DOCUMENT ME! */
    private int whitespaceW = 10;

    /** DOCUMENT ME! */
    private float[] yLocHistory; // history of the y-locatioss

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This is the history graph, and sets up and animation thread. The history will be large enough to display samples
     * across the entire screen-size
     */
    public LineMeter() {
        setBackground(Color.black);
        // setBorder(BorderFactory.createMatteBorder(20, 20, 20, 20, this.getBackground()));

        samplesPerUpdate = 1;
        numberOfDivisions = 15;

        // the history must be large enough to allow display across whole screen
        yLocHistory = new float[Toolkit.getDefaultToolkit().getScreenSize().width / pixelsWidePerSample];
        recentSamples = new float[samplesPerUpdate];

        for (int i = 0; i < yLocHistory.length; i++) { // preset the history
            yLocHistory[i] = 1; // to draw at the bottom of the graph
        }

        setOpaque(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * removes the samples from the buffer and transfers them to the history buffer, and resets the recent history to
     * capture new samples.
     */
    public final void flushRecentSamples() {
        System.arraycopy(yLocHistory, countSinceLastUpdate, yLocHistory, 0, yLocHistory.length - countSinceLastUpdate);
        System.arraycopy(recentSamples, 0, yLocHistory, yLocHistory.length - 1 - countSinceLastUpdate,
                         countSinceLastUpdate);
        countSinceLastUpdate = 0;
    }

    /**
     * returns the panels maximum size.
     *
     * @return  DOCUMENT ME!
     */
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }


    /**
     * returns the displays minimum size.
     *
     * @return  DOCUMENT ME!
     */
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    /**
     * returns the panels preferred size.
     *
     * @return  DOCUMENT ME!
     */
    public Dimension getPreferredSize() {
        return new Dimension(200, 120);
    }


    /**
     * repaints the history graph.
     *
     * @param  g  DOCUMENT ME!
     */
    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        if (g == null) {
            return;
        }

        int i, sample = 0, hLoc, x1, x2, y1, y2;
        insets = getInsets();
        dim = getSize();
        g2d = (Graphics2D) g;

        if ((panelWidth != dim.width) || (panelHeight != dim.height)) {
            panelWidth = dim.width;
            panelHeight = dim.height;

            drawingWidth = panelWidth - insets.left - insets.right;
            drawingHeight = panelHeight - insets.bottom - insets.top;

            chartWidth = drawingWidth - (2 * whitespaceW);
            chartHeight = drawingHeight - (2 * whitespaceH);

            chartXstart = insets.left + whitespaceW;
            chartXend = chartXstart + chartWidth;
            chartYstart = insets.top + whitespaceH;
            chartYend = chartYstart + chartHeight;

            lineSpacing = Math.round((float) chartHeight / 10);

            // draw the horizontal lines
            numberOfSamplesDisplayed = chartWidth / pixelsWidePerSample;
            numberOfSamplesPerDivision = numberOfSamplesDisplayed / numberOfDivisions;
            //pixelsPerDivision = pixelsWidePerSample * numberOfSamplesPerDivision;
        }

        // draw horizontal lines
        g2d.setColor(lineColor);

        for (i = 0; i < 10; i++) {
            y1 = chartYstart + (i * lineSpacing) + 1;
            g2d.drawLine(chartXstart, y1, chartXend, y1);
        }

        y1 = chartYstart + (i * lineSpacing);
        g2d.drawLine(chartXstart, y1, chartXend, y1);


        hLoc = yLocHistory.length - numberOfSamplesDisplayed;

        x1 = chartXstart;
        x2 = chartXstart + pixelsWidePerSample;
        y1 = (int) (chartHeight * yLocHistory[hLoc]) + insets.bottom + whitespaceH;
        y2 = (int) (chartHeight * yLocHistory[hLoc + 1]) + insets.bottom + whitespaceH;

        g2d.setColor(penColor);

        for (++hLoc; hLoc < (yLocHistory.length - 2); hLoc++) {

            // if this is a place for a vertical line...draw the moving vertical lines
            if (sample == numberOfSamplesPerDivision) {
                g2d.setColor(lineColor);
                g2d.drawLine(x1 + animationFrame, chartYstart, x1 + animationFrame, chartYend);
                sample = 0;
                g2d.setColor(penColor);
            }

            g2d.drawLine(x1, y1, x2, y2);

            x1 += pixelsWidePerSample;
            y1 = (int) (chartHeight * yLocHistory[hLoc]) + insets.bottom + whitespaceH;
            x2 += pixelsWidePerSample;
            y2 = (int) (chartHeight * yLocHistory[hLoc + 1]) + insets.bottom + whitespaceH;

            sample++;
        }

        g2d.drawLine(x1, y1, x2, y2);

    }

    /**
     * when the thread wakes up, if the panel is showing, it will repaint panel, scrolling the graph.
     */
    public void run() {
        Thread me = Thread.currentThread();

        while (((animationThread == me) && !isShowing()) || (getSize().width == 0)) {

            try {
                Thread.sleep(500); // chk back every 1/2sec to see if showing
            } catch (InterruptedException e) {
                return;
            }
        }

        while ((animationThread == me) && isShowing()) {

            // if there were no updates into history, use the most recent history position
            counterLock = true;

            if (countSinceLastUpdate == 0) { // CAUTION: if entering here, but update comes in between here and repaint
                                             // this will CRASH!
                recentSamples[0] = yLocHistory[yLocHistory.length - 2];
                countSinceLastUpdate++;
            }

            // flushRecentSamples();     // the following does the method inline to save time
            try {
                System.arraycopy(yLocHistory, countSinceLastUpdate, yLocHistory, 0,
                                 yLocHistory.length - countSinceLastUpdate);
                System.arraycopy(recentSamples, 0, yLocHistory, yLocHistory.length - 1 - countSinceLastUpdate,
                                 countSinceLastUpdate);
                countSinceLastUpdate = 0;
            } catch (ArrayIndexOutOfBoundsException aioobe) { }
            finally {
                counterLock = false; // turn off the locking mechanism to allow buffer to be filled
            }

            repaint();

            animationFrame -= pixelsWidePerSample * samplesPerUpdate;

            if (animationFrame <= 0) {
                animationFrame = (pixelsWidePerSample) * (numberOfSamplesPerDivision);
            }

            try {
                Thread.sleep(aniSampleRate);
            } catch (InterruptedException e) {
                break;
            }
        }

        animationThread = null;
    }

    /**
     * Sets the most recent point on the history buffer.
     *
     * @param  amp  -- floating point value between (and including) 0 and 1, which represents the y-coordinate on the
     *              graph. It is then adjusted to be drawn based on display size.
     */
    public void setAmplitude(float amp) {

        if ((amp < 0) || (amp > 1)) {
            return;
        }

        try {

            if (!counterLock) {
                recentSamples[countSinceLastUpdate] = 1 - amp; // draws start zero from top, amp starts from bottom
                countSinceLastUpdate++;
            }
        } catch (ArrayIndexOutOfBoundsException aioobe) { } // skip sample
    }

    /**
     * applies the given color to bars which are "lit" and are up to the set amplitude.
     *
     * @param  c  DOCUMENT ME!
     */
    public void setLineChartColor(Color c) {
        lineColor = c;
    }

    /**
     * applies the given color to the background. Thre same as setBackground()
     *
     * @param  c  DOCUMENT ME!
     */
    public void setPaperColor(Color c) {
        paperColor = c;
        setBackground(paperColor);
    }

    /**
     * applies the given color to bars which are not lit and are above the set amplitude.
     *
     * @param  c  DOCUMENT ME!
     */
    public void setPenColor(Color c) {
        penColor = c;
    }

    /**
     * adjusts the number of samples to be sent to the history in milliseconds. This allows for a recent buffer for
     * samples not yet displayed. Changes to the sample rate will flush the current set of updates to the history
     * buffer.
     *
     * @param  samRate  -- number of milliseconds per sample.
     */
    public void setSampleRate(int samRate) { // setting sample rate to add points into history (in ms)
        sampleRate = samRate;
        samplesPerUpdate = aniSampleRate / sampleRate; // sampleRate/aniSampleRate;

        if (samplesPerUpdate == 0) {
            samplesPerUpdate = 1;
        }

        flushRecentSamples(); // old samples into history buffer
        recentSamples = new float[samplesPerUpdate];
    }

    /**
     * sets the animation/redraw thread up, then runs it.
     */
    public void start() {
        animationThread = new Thread(this);
        animationThread.setPriority(Thread.MIN_PRIORITY);
        animationThread.setName("MonitorAnimation");
        animationThread.start();
    }

    /**
     * shuts the animation thread off.
     */
    public synchronized void stop() {
        animationThread = null;
        notify();
    }

}
