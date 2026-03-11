//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver;

import de.jcup.sarif_2_1_0.model.Exception;
import de.jcup.sarif_2_1_0.model.*;
import java.util.List;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

public class SarifReflectionFeature implements Feature {

    @Override
    public void beforeAnalysis(BeforeAnalysisAccess access) {
        // List all classes to register for runtime reflection
        var classToRegister = List.of(
            SarifSchema210.class,
            SarifSchema210.Version.class,
            Invocation.class,
            Run.class,
            Tool.class,
            ToolComponent.class,
            ReportingDescriptor.class,
            ReportingConfiguration.class,
            ReportingConfiguration.Level.class,
            ReportingDescriptorRelationship.class,
            ReportingDescriptorReference.class,
            Result.class,
            Notification.class,
            Notification.Level.class,
            Exception.class,
            Stack.class,
            StackFrame.class,
            Message.class,
            MultiformatMessageString.class,
            Location.class,
            PhysicalLocation.class,
            LogicalLocation.class,
            ArtifactLocation.class,
            Region.class,
            PropertyBag.class
        );

        // Then register them
        for (var clazz : classToRegister) {
            RuntimeReflection.register(clazz);
            RuntimeReflection.register(clazz.getDeclaredFields());
            RuntimeReflection.register(clazz.getDeclaredConstructors());
            RuntimeReflection.register(clazz.getDeclaredMethods());
        }
    }
}
