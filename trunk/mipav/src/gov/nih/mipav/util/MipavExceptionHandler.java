package gov.nih.mipav.util;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.reportbug.ReportBugBuilder;
import gov.nih.mipav.view.dialogs.reportbug.ReportBugBuilder.BugType;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;


public class MipavExceptionHandler implements Thread.UncaughtExceptionHandler {
    @Override
    public void uncaughtException(final Thread thread, final Throwable e) {
        final String summary = "New automatic error/exception reporting";
        String name = ReportBugBuilder.getNameDefault();
        if (name == null) {
            name = "Anonymous user";
        }
        String email = ReportBugBuilder.getEmailDefault();
        if (email == null) {
            email = "bug@mipav.cit.nih.gov";
        }
        final String version = MipavUtil.getVersion();
        final String os = System.getProperties().getProperty("os.name") + " - " + System.getProperty("os.arch");
        final String urgency = "Low";
        final String description = getStackTrace(e);
        final BugType bugType = BugType.AUTOMATIC_ERROR_REPORTING;

        final String workingDir = System.getProperty("user.dir");
        final String jreDir = System.getProperty("java.home");
        final String[] javaClassPath = System.getProperties().getProperty("java.class.path").split(";");

        // if the jre dir is under the working dir, mipav is probably running off an install instead of dev environment
        // JWS appears to set the classpath to only point to deploy.jar in the JRE, so we use that to detect JWS
        // execution
        if (jreDir.startsWith(workingDir) || javaClassPath.length == 1) {
            ReportBugBuilder.sendReportWeb(summary, name, email, version, os, urgency, description, bugType, new ArrayList<String>(), new ArrayList<String>());
        } else {
            System.err.println(ReportBugBuilder.compileReport(summary, name, email, version, os, urgency, description));
        }

        // also output to console
        e.printStackTrace();
    }

    public static String getStackTrace(final Throwable e) {
        final Writer result = new StringWriter();
        final PrintWriter printWriter = new PrintWriter(result);
        e.printStackTrace(printWriter);
        return result.toString();
    }
}
