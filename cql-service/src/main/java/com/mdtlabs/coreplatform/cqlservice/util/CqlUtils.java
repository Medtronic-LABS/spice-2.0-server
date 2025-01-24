package com.mdtlabs.coreplatform.cqlservice.util;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import ca.uhn.fhir.context.FhirContext;
import ca.uhn.fhir.context.FhirVersionEnum;
import ca.uhn.fhir.rest.client.api.IGenericClient;
import ca.uhn.fhir.rest.client.interceptor.SimpleRequestHeaderInterceptor;
import org.cqframework.cql.cql2elm.CqlTranslatorOptions;
import org.cqframework.cql.cql2elm.ModelManager;
import org.cqframework.cql.cql2elm.model.Model;
import org.hl7.elm.r1.VersionedIdentifier;
import org.hl7.fhir.r4.model.Bundle;
import org.opencds.cqf.cql.engine.execution.LibraryLoader;
import org.opencds.cqf.cql.evaluator.cql2elm.content.InMemoryLibraryContentProvider;
import org.opencds.cqf.cql.evaluator.cql2elm.content.LibraryContentProvider;
import org.opencds.cqf.cql.evaluator.cql2elm.content.fhir.EmbeddedFhirLibraryContentProvider;
import org.opencds.cqf.cql.evaluator.cql2elm.model.CacheAwareModelManager;
import org.opencds.cqf.cql.evaluator.engine.execution.TranslatingLibraryLoader;
import org.opencds.cqf.cql.evaluator.fhir.DirectoryBundler;
import org.springframework.stereotype.Component;

import com.mdtlabs.coreplatform.commonservice.common.Constants;
import com.mdtlabs.coreplatform.commonservice.common.exception.DataNotFoundException;
import com.mdtlabs.coreplatform.commonservice.common.logger.Logger;
import com.mdtlabs.coreplatform.cqlservice.constants.CqlConstants;

/**
 * <p>
 * The CqlUtils class provides methods to manipulate the cql files.
 * </p>
 *
 * @author Vishwaeaswaran M created at May 21, 2024.
 */
@Component
public class CqlUtils {

    private static FhirContext fhirContext = null;

    private CqlUtils() {
    }

    /**
     * <p>
     * This method is used to create a new FHIR context.
     * </p>
     *
     * @return the new FHIR context.
     */
    public static FhirContext getFhirContext() {
        if (Objects.isNull(fhirContext)) {
            fhirContext = FhirContext.forCached(FhirVersionEnum.R4);
        }
        return fhirContext;
    }

    /**
     * <p>
     * This method is used to create a new FHIR client instance.
     * </p>
     *
     * @param url         The URL to connect to the Fhir server.
     * @param clientValue The client value for the header.
     * @param token       The token to use for authentication.
     * @return the new FHIR client instance.
     */
    public static IGenericClient getFhirClient(String url, String clientValue, String token) {
        FhirContext fhirContext = getFhirContext();
        // Create a FHIR client
        IGenericClient client = fhirContext.newRestfulGenericClient(url);
        client.registerInterceptor(new SimpleRequestHeaderInterceptor(Constants.CLIENT, clientValue));
        client.registerInterceptor(new SimpleRequestHeaderInterceptor(Constants.AUTHORIZATION, token));
        return client;
    }

    /**
     * Get id value form history URL
     *
     * @param url Reference History URL
     * @return Id value
     */
    public static String getIdFromHistoryUrl(String url) {
        String[] paths = url.split(CqlConstants.FHIR_HISTORY_ID_REGEX)[Constants.ZERO].split(Constants.FORWARD_SLASH);
        return paths[paths.length - Constants.ONE];
    }

    /**
     * Get id value form Reference
     *
     * @param reference Reference
     * @return Id value
     */
    public static String getIdFromReference(String reference) {
        String[] referencePaths = reference.split(Constants.FORWARD_SLASH);
        return referencePaths[referencePaths.length - Constants.ONE];
    }

    /**
     * <p>
     * This method is used to get the terminology resource bundle.
     * </p>
     *
     * @return the terminology resource bundle.
     */
    public static Bundle getTerminologyResourceBundle(String valuesetPath) {
        Logger.logInfo("Start to fetch Terminology Resources Bundle");
        DirectoryBundler bundler = new DirectoryBundler(getFhirContext());
        return (Bundle) bundler.bundle(valuesetPath);
    }

    /**
     * <p>
     * This method is used to get the cql files.
     * </p>
     *
     * @return list of cql files content.
     */
    public static List<String> getCqlFiles(String cqlPath) {
        Logger.logInfo("Start to fetch CQL Files");
        try (Stream<Path> stream = Files.list(Paths.get(cqlPath))) {
            return stream.filter(filePath -> filePath.toFile().getName().endsWith(CqlConstants.CQL_EXTENSION))
                    .map(CqlUtils::readAllBytesToString)
                    .filter(content -> Objects.nonNull(content) && !content.isEmpty()).toList();
        } catch (Exception exception) {
            throw new DataNotFoundException(1051);
        }
    }

    /**
     * <p>
     * This method is used to create a LibraryLoader for the given libraries.
     * </p>
     *
     * @param libraries the libraries to build the library loader.
     * @return the library loader.
     */
    public static LibraryLoader buildLibraryLoader(List<String> libraries) {
        // Set up LibraryLoader that translates from CQL
        Map<VersionedIdentifier, Model> modelCache = new HashMap<>();
        ModelManager modelManager = new CacheAwareModelManager(modelCache);
        List<LibraryContentProvider> contentProviders = new ArrayList<>();
        contentProviders.add(new InMemoryLibraryContentProvider(libraries));
        contentProviders.add(new EmbeddedFhirLibraryContentProvider());
        return new TranslatingLibraryLoader(modelManager, contentProviders, CqlTranslatorOptions.defaultOptions());
    }

    /**
     * <p>
     * This method is reads all content from the specified file path.
     * </p>
     *
     * @param filePath the path to read the file.
     * @return the content of the specified file.
     */
    private static String readAllBytesToString(Path filePath) {
        String content = "";
        try {
            content = new String(Files.readAllBytes(filePath));
        } catch (IOException e) {
            Logger.logError(e.getMessage());
        }
        return content;
    }
}
