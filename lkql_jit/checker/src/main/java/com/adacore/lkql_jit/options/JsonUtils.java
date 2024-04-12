//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.options;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.util.Map;
import java.util.stream.Collectors;

/** This class contains all util functions to handle JSON */
public final class JsonUtils {

    /** Serialize the given rule instance map to a JSON string. */
    public static String serializeInstances(final Map<String, RuleInstance> instances) {
        final JSONObject res = new JSONObject();
        for (var instance : instances.entrySet()) {
            res.put(instance.getKey(), instance.getValue().toJson());
        }
        return res.toString();
    }

    /**
     * Deserialize the given JSON source to a rule instance map.
     *
     * @throws JSONException If the given source cannot be deserialized.
     */
    public static Map<String, RuleInstance> deserializeInstances(final String jsonSources)
            throws JSONException {
        JSONObject jsonInstances = new JSONObject(new JSONTokener(jsonSources));
        return jsonInstances.keySet().stream()
                .collect(
                        Collectors.toMap(
                                s -> s,
                                s -> RuleInstance.fromJson(jsonInstances.getJSONObject(s))));
    }
}
