package gov.nih.mipav.util;


import java.util.concurrent.*;


public class ThreadUtil {
    public static final int nthreads = 2 * ThreadUtil.getAvailableCores();

    public static Executor mipavThreadPool = Executors.newFixedThreadPool(ThreadUtil.nthreads);

    /**
     * Return the available processors in your machine. TODO: Add a similar method for GPUs once such a method exists in
     * Java
     */
    public static int getAvailableCores() {
        return Runtime.getRuntime().availableProcessors();
    }
}
