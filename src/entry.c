/**
 * entry.c -- Source code for passing Godot-assigned pointers to Clojure.
 *
 * The main function is `godot_entry` which is called by Godot via GDExtension.
 *
 * The process is rather simple. We take the classpath from env, initialize JVM,
 * take some Clojure functions that can invoke any Clojure stuff and eventually
 * call a handler function on Clojure side.
 *
 * This file contains several helpers to make the communication cleaner.
 *
 * These links will help you understand this file better:
 * - https://clojure.org/reference/java_interop#_calling_clojure_from_java [2024-03-01]
 *   Describes how Clojure can be called from Java.
 *
 * - https://docs.oracle.com/en/java/javase/11/docs/specs/jni/invocation.html [2024-03-01]
 *   Describes how to call Java from C.
 *
 * - https://stackoverflow.com/questions/8066253/compute-a-java-functions-signature [2024-03-01]
 *   Explains how to construct a signature string which is used by functions like
 *   GetStaticMethodID to choose a correct method. This will also give a better
 *   understanding of *_SIG constants.
 *
 * Also make sure to read <jni.h> which contains all available JNI functions.
 */

#include <stdio.h>
#include <jni.h>
#include <stdlib.h>
#include "../godot-headers/gdextension_interface.h"

#define J_STR_CLASS "java/lang/String"
#define J_OBJ_CLASS "java/lang/Object"
#define CLJ_IFN_CLASS "clojure/lang/IFn"
#define CLJ_CLJ_CLASS "clojure/java/api/Clojure"

// Turn class name into function/method signature representation
#define SIG_C(C) "L" C ";"
#define VAR_SIG     "(" SIG_C(J_OBJ_CLASS) SIG_C(J_OBJ_CLASS) ")" SIG_C(CLJ_IFN_CLASS)
#define READ_SIG    "(" SIG_C(J_STR_CLASS) ")"                    SIG_C(J_OBJ_CLASS)
#define INVOKE1_SIG "(" SIG_C(J_OBJ_CLASS) ")"                    SIG_C(J_OBJ_CLASS)

#define HALT_ON_ERR(env) if (!assert_no_errors(env)) return 1;

typedef enum { false, true } bool;

void noop(void *userdata, GDExtensionInitializationLevel p_level) {
  return;
}

/**
 * If error has occurred, report it and return false, otherwise return true.
 */
bool assert_no_errors(JNIEnv *env) {
  if ((*env)->ExceptionOccurred(env)) {
    (*env)->ExceptionDescribe(env);
    return false;
  }
  return true;
}

jobject new_integer(JNIEnv* env, jint value){
  jclass cls = (*env)->FindClass(env, "java/lang/Integer");
  jmethodID methodID = (*env)->GetMethodID(env, cls, "<init>", "(I)V");
  return (*env)->NewObject(env, cls, methodID, value);
}

jobject new_long(JNIEnv* env, jlong value) {
  jclass cls = (*env)->FindClass(env, "java/lang/Long");
  jmethodID methodID = (*env)->GetMethodID(env, cls, "<init>", "(J)V");
  return (*env)->NewObject(env, cls, methodID, value);
}

jobject new_string(JNIEnv *env, const char *str) {
  return (*env)->NewStringUTF(env, str);
}

/**
 * Return a freshly allocated formatted char*.
 */
char *new_sprintf(const char *format, ...) {
  va_list fargs0, fargs1;
  va_start(fargs0, format);
  va_copy(fargs1, fargs0);

  size_t buflen = vsnprintf(NULL, 0, format, fargs0) + 1;
  char *res = malloc(sizeof(char) * buflen);
  vsnprintf(res, buflen, format, fargs1);

  va_end(fargs0);
  va_end(fargs1);
  return res;
}
typedef struct {
  JNIEnv* env;

  // See CLJ_CLJ_CLASS
  jclass clj_class;
  // See VAR_SIG
  jmethodID var_method_id;
  // See READ_SIG
  jmethodID read_method_id;

  // See CLJ_IFN_CLASS
  jclass ifn_class;
  // See INVOKE1_SIG . We hardcoded this one invoke function form, there's actually
  // 22 of them: 0-20 args and 21+ args
  jmethodID invoke1;
} ClojureGlue;

/**
 * Initialize clj_class.
 */
bool init_clojure_glue(JNIEnv* env, ClojureGlue *clj) {
  clj->env = env;
  clj->clj_class = (*env)->FindClass(env, CLJ_CLJ_CLASS);
  if (!assert_no_errors(env)) return false;

  clj->var_method_id = (*env)->GetStaticMethodID(env, clj->clj_class, "var", VAR_SIG);
  if (!assert_no_errors(env)) return false;

  clj->read_method_id = (*env)->GetStaticMethodID(env, clj->clj_class, "read", READ_SIG);
  if (!assert_no_errors(env)) return false;

  clj->ifn_class = (*env)->FindClass(env, CLJ_IFN_CLASS);
  if (!assert_no_errors(env)) return false;

  clj->invoke1 = (*env)->GetMethodID(env, clj->ifn_class, "invoke", INVOKE1_SIG);
  if (!assert_no_errors(env)) return false;

  return true;
}

/**
 * Wrapper function for `Clojure.var(java.lang.Object ns, java.lang.Object name)`.
 */
jobject clj_var(ClojureGlue *clj, const char *ns, const char *name) {
  return (*clj->env)->CallStaticObjectMethod(clj->env,
                                             clj->clj_class,
                                             clj->var_method_id,
                                             new_string(clj->env, ns),
                                             new_string(clj->env, name));
}

/**
 * Wrapper function for `Clojure.read(java.lang.String s)`.
 */
jobject clj_read(ClojureGlue *clj, const char *s) {
  return (*clj->env)->CallStaticObjectMethod(clj->env,
                                             clj->clj_class,
                                             clj->read_method_id,
                                             new_string(clj->env, s));
}

/**
 * Wrapper function for `IFn.invoke(java.lang.Object)`
 */
jobject ifn_invoke1(ClojureGlue *clj, jobject instance, jobject arg1) {
  return (*clj->env)->CallObjectMethod(clj->env, instance, clj->invoke1, arg1);
}

// Make sure you open godot from console in order to see these printf's
// also you should be at Godot 4.1+ afaik
GDExtensionBool
godot_entry(
  GDExtensionInterfaceGetProcAddress p_get_proc_address,
  const GDExtensionClassLibraryPtr p_library,
  GDExtensionInitialization *r_initialization
) {
  JavaVM *jvm;
  JNIEnv *env;
  JavaVMInitArgs vm_args;
  JavaVMOption option;

  const char *classpath = getenv("CLASSPATH");
  if (!classpath) {
    fprintf(stderr, "CLASSPATH is not defined!\n");
    return 1;
  }

  option.optionString = new_sprintf("%s%s", "-Djava.class.path=", classpath);
  vm_args.options = &option;
  vm_args.nOptions = 1;
  vm_args.version = JNI_VERSION_10;
  vm_args.ignoreUnrecognized = false;

  JNI_CreateJavaVM(&jvm, (void **)&env, &vm_args);
  HALT_ON_ERR(env);

  ClojureGlue clj;
  if (!init_clojure_glue(env, &clj)) return 1;

  const char *entry_ns = "godot-clojure.core";

  jobject entry_callback = clj_var(&clj, entry_ns, "entry-point");
  HALT_ON_ERR(env);

  jclass require = clj_var(&clj, "clojure.core", "require");
  HALT_ON_ERR(env);

  jobject entry_ns_obj = clj_read(&clj, entry_ns);
  HALT_ON_ERR(env);

  // We must import the namespace before we can call the functions inside it
  ifn_invoke1(&clj, require, entry_ns_obj);
  HALT_ON_ERR(env);

  ifn_invoke1(&clj, entry_callback, new_long(env, (jlong)p_get_proc_address));
  HALT_ON_ERR(env);


  GDExtensionGodotVersion version_info;
  GDExtensionInterfaceGetGodotVersion get_godot_version
    = (GDExtensionInterfaceGetGodotVersion)p_get_proc_address("get_godot_version");
  get_godot_version(&version_info);

  printf("Godot Version info:\n");
  printf("  major: %i\n", version_info.major);
  printf("  minor: %i\n", version_info.minor);
  printf("  patch: %i\n", version_info.patch);
  printf("  str: %s\n", version_info.string);

  // These functions have to be initialized to prevent crashing
  r_initialization->initialize = noop;
  r_initialization->deinitialize = noop;

  // TODO Do we need to free JVM?

  return 1;
}
