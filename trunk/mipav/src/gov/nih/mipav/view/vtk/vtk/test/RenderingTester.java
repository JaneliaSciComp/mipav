package gov.nih.mipav.view.vtk.vtk.test;

import EDU.oswego.cs.dl.util.concurrent.WaitableBoolean;
import junit.framework.TestCase;
import vtk.util.VtkPanelContainer;
import vtk.vtkImageDifference;
import vtk.vtkPNGReader;
import vtk.vtkPanel;
import vtk.vtkTIFFReader;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.util.Observable;
import java.util.Observer;

/**
 * Utility class to test is a a rendering performed within a vtkPanel object.
 * The testing is performed by specifying a class that when instatiated can
 * return a reference to tested vtkPanel. That class is assumed to implement
 * interface {@link VtkPanelContainer}.<p/>
 * <p/>
 * Tolerance on difference between baseline and a test image can be controlled
 * by setting <code>test.tolerance</code> system property. For example, to
 * set it to <code>2.5</code> use call to <code>System.setProperty</code>:
 * <pre>
 *   System.setProperty("test.tolerance", "2.5")
 * </pre>
 *
 * @author Jarek Sacha
 * @version $ Revision: $
 */

public class RenderingTester extends TestCase {

  private Class containerClass;
  private double errorTolerance;


  private static final String DUMP_DIR = "dump" + File.separator;
  private static final String BASELINE_DIR = "data" + File.separator + "baseline" + File.separator;
  private static final String DUMP_FILE_EXNTENTION = ".tif";
  private static final String BASELINE_FILE_EXNTENTION = ".png";

  private static final String TEST_TOLERANCE_PROPERTY = "test.tolerance";
  private static final double TEST_TOLERANCE_DEFAULT = 0.1;

  /**
   * Maximum wait time in milliseonds.
   */
  private static final long MAX_WAIT = 10000;

  /**
   * Constructor.
   * 
   * @param containerClass a class that when instatiated can
   *                       return a reference to the tested vtkPanel.
   */
  public RenderingTester(Class containerClass) {
    super("testRendering");
    this.containerClass = containerClass;
    this.errorTolerance = getErrorTolerance();
  }

  /**
   * Perform testing. If test is test is successful the method simply returns.
   * If test fails a related exception is thrown.
   * 
   * @throws Exception when testing fails or an error occured.
   */
  public void testRendering() throws Exception {
    compareToBaseline(containerClass);
  }

  /*
   *
   */
  private void compareToBaseline(Class classToTest)
      throws
      InstantiationException,
      IllegalAccessException,
      InterruptedException,
      InvocationTargetException {

    Object objectToTest = classToTest.newInstance();
    assertTrue("Class '" + objectToTest.getClass().getName()
        + "' does not extend/implement '" + VtkPanelContainer.class + "'.",
        objectToTest instanceof VtkPanelContainer);

    final VtkPanelContainer container = (VtkPanelContainer) objectToTest;

    JFrame frame;
    frame = new JFrame(container.getClass().getName());
    frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    frame.getContentPane().add("Center", (Component) container);
    frame.pack();
    frame.setVisible(true);


    // Give some extra chance for the frame to render
    Thread.yield();

    // Dump VTK content of render window
    String dumpImageName = DUMP_DIR + container.getClass().getName() + DUMP_FILE_EXNTENTION;
    hardCopyWhenWindowSet(dumpImageName, container.getRenWin());

    // Close frame as it is no longer needed.
    frame.setVisible(false);
    frame = null;

    // Check in dump file exist
    File dumpImageFile = new File(dumpImageName);
    assertTrue("Testing if dump image exist: " + dumpImageFile.getAbsolutePath(),
        dumpImageFile.exists());

    // Setup dump reader
    vtkTIFFReader dumpReader = new vtkTIFFReader();
    dumpReader.SetFileName(dumpImageName);
    dumpReader.Update();

    // Check if baseliune exists
    String baselineImageName = BASELINE_DIR + container.getClass().getName() + BASELINE_FILE_EXNTENTION;
    File baselineImageFile = new File(baselineImageName);
    assertTrue("Baseline image does not exist: " + baselineImageFile.getAbsolutePath(),
        baselineImageFile.exists());

    // Setup baseline reader
    vtkPNGReader baselineReader = new vtkPNGReader();
    baselineReader.SetFileName(baselineImageName);
    baselineReader.Update();

    // Get image difference
    vtkImageDifference imageDifference = new vtkImageDifference();
    imageDifference.SetInput(dumpReader.GetOutput());
    imageDifference.SetImage(baselineReader.GetOutput());
    imageDifference.Update();

    double error = imageDifference.GetThresholdedError();

    assertEquals("Testing class '" + classToTest.getName() +
        "': image difference above tolerance,", 0, error, errorTolerance);
  }

  static double getErrorTolerance() {
    // set default value
    double errorTolerance = TEST_TOLERANCE_DEFAULT;

    // See if TEST_TOLERANCE_PROPERTY is defined, if so try us it as errorTolerance
    String testToleranceProperty = System.getProperty(TEST_TOLERANCE_PROPERTY);
    if (testToleranceProperty != null) {
      try {
        errorTolerance = Double.parseDouble(testToleranceProperty);
      } catch (NumberFormatException e) {
        throw new RuntimeException("Unable to convert system property '"
            + TEST_TOLERANCE_PROPERTY + "' to double ["
            + TEST_TOLERANCE_PROPERTY + testToleranceProperty + "].");
      }
    }

    return errorTolerance;
  }

  /**
   * Wait (block) till <code>vtkPanel</code>'s property
   * </code>windowSet</code> is <code>true</code>.
   * 
   * @param renWin vtkPanel to wait for.
   * @throws InterruptedException  if another thread has interrupted the current thread
   *                               while waiting for value of </code>windowSet</code> to <code>true</code>.
   * @throws IllegalStateException when <code>MAX_WAIT</time> is reached and
   *                               </code>windowSet</code> is still <code>false</code>.
   */
  private static void waitTillWindowSet(final vtkPanel renWin)
      throws InterruptedException, IllegalStateException {
    // Use 'synchronized' to lock access to 'renWin'
    // Without locking, it is possible that state of 'windowSetSync'
    // could change from 'false' to 'true' after call to 'isWindowSet()'
    // but before we are able to add an observer. It that was the case we will
    // never receive notification that 'windowSetSync' is 'true'.


    // Create a WaitableBoolean variable with initial value 'false' and its lock
    // being the 'renWin'. WaitableBoolean issues notification whenever its value
    // changes.
    final WaitableBoolean windowSetSync = new WaitableBoolean(false, renWin);

    // Add observer that will update windowSetSync when ever 'windowSet'
    // property of 'renWin' is updated.
    renWin.addWindowSetObserver(new Observer() {
      public void update(Observable o, Object arg) {
        final boolean value = renWin.isWindowSet();
        if (value != windowSetSync.get()) {
          windowSetSync.set(value);
        }
      }
    });

    // Lock windowSetSync while checking if windowSet is 'true'.
    // If 'false' wait for windowSet to change to true for the
    // 'MAX_WAIT' milliseconds.
    Object lock = windowSetSync.getLock();
    synchronized (lock) {
      windowSetSync.set(renWin.isWindowSet());
      if (!windowSetSync.get()) {
        lock.wait(MAX_WAIT);

        if (!windowSetSync.get()) {
          throw new IllegalStateException("Maximum time, " + MAX_WAIT
              + "ms, reached when waiting for 'windowSetSync' property of "
              + renWin.getClass().getName() + ". The value is still false.");
        }
      }
    }
  }

  /**
   * Wait (block) till <code>vtkPanel</code>'s property  </code>windowSet</code>
   * is <code>true</code> then write hard copy of the vtkPanel content to a TIFF file.
   * 
   * @param fileName name of the file where to write vtkPanel content.
   * @param renWin   vtkPanel to get hard copy from.
   * @throws InterruptedException 
   * @throws java.lang.reflect.InvocationTargetException
   *                              
   */
  static void hardCopyWhenWindowSet(final String fileName,
                                    final vtkPanel renWin)
      throws InterruptedException, InvocationTargetException {

    // Wait till vtkPanel completed rendering for the first time.
    waitTillWindowSet(renWin);

    // Dump content of vtkPanel executing code within AWT Event thread.
    SwingUtilities.invokeAndWait(new Runnable() {
      public void run() {
        final int magnification = 1;
        renWin.HardCopy(fileName, magnification);
      }
    });
  }
}