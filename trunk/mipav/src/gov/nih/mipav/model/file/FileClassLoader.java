package gov.nih.mipav.model.file;


import gov.nih.mipav.plugins.*;

import java.io.*;

import java.net.*;


/**
 * MIPAV uses this class loader to load plug-ins from the plugins folder. It's based on the class of the same name from
 * the "Java Class Libraries: Second Edition, Vol. 1" (http://java.sun.com/docs/books/chanlee/second_edition/vol1/). and
 * Wayne Rasband developer of NIHImage and ImageJ. Might be a problem with static variables in the plugin. I don't use
 * this code at this time.
 *
 * @version  1.0
 */
public class FileClassLoader extends ClassLoader {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private String classPath;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * FileClassLoader - loads a class file in the form of a plug-in.
     *
     * @param  path  class path of the class
     */
    public FileClassLoader(String path) {
        classPath = path;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * loadIt - loads the bytes from file.
     *
     * @param   className  name of class to be loaded into MIPAV - WITHOUT ".class"
     *
     * @return  DOCUMENT ME!
     *
     * @throws  ClassNotFoundException  DOCUMENT ME!
     */
    public Class loadIt(String className) throws ClassNotFoundException {

        // To get file name, remove the package name, if any.
        String fileName;
        int bufferSize;
        byte[] buffer;
        InputStream inputStream = null;

        fileName = className + ".class";

        File fullName = new File(classPath, fileName);

        try {

            // Read in the byte codes.
            inputStream = new FileInputStream(fullName);
            bufferSize = (int) fullName.length();
            buffer = new byte[bufferSize];

            inputStream.read(buffer, 0, bufferSize);
            inputStream.close();

            // Define the class
            return defineClass(className, buffer, 0, buffer.length);
        } catch (Exception e) {
            throw new ClassNotFoundException(className);
        }
    }

    /**
     * loadClass - loads a class, my MIPAV case a "PlugIn"
     *
     * @param   name     name of class
     * @param   resolve  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @throws  ClassNotFoundException  DOCUMENT ME!
     */
    protected synchronized Class loadClass(String name, boolean resolve) throws ClassNotFoundException {

        // Try to find it from the cache
        Class c = findLoadedClass(name);

        // Not in cache
        if (c == null) {

            // See if it can be loaded by system class loader
            try {

                // No need to call resolveClass() on the result
                // because findSystemClass() loads and links the class.
                return findSystemClass(name);
            } catch (ClassNotFoundException e) { }

            // Try to get it from file
            c = loadIt(name);
        }

        // Link class if asked to do so
        if ((c != null) && resolve) {
            resolveClass(c);
        }

        return c;
    }
}
