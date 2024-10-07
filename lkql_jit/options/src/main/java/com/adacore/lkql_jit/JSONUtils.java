//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit; //

import java.util.Map;
import java.util.stream.Collectors;
import org.json.JSONObject;

/** This class contains all util functions to handle JSON */
public final class JSONUtils {
    /** Given a JSON object, parse it as a String to String map and return the result. */
    public static Map<String, String> parseStringMap(JSONObject jsonObject) {
        return jsonObject.toMap().entrySet().stream()
                .map(e -> Map.entry(e.getKey(), (String) e.getValue()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }
}
