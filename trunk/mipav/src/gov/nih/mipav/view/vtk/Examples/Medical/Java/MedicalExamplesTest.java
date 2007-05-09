package gov.nih.mipav.view.vtk.Examples.Medical.Java;
import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import gov.nih.mipav.view.vtk.vtk.test.RenderingTester;

/**
 * @author Jarek Sacha
 * @version $ Revision: $
 */

public class MedicalExamplesTest extends TestCase {


  /**
   * Create custom test suite for Modelling examples.
   * 
   * @return Test suite for Modelling examples.
   */
  public static Test suite() {

    // Example classes that will be tested
    Class[] examplesToTest = {
      Medical1.class,
      Medical2.class,
      Medical3.class
    };

    // Create a test suite
    TestSuite suite = new TestSuite();
    for (int i = 0; i < examplesToTest.length; i++) {
      Class exampleClass = examplesToTest[i];
      suite.addTest(new RenderingTester(exampleClass));
    }

    return suite;
  }
}