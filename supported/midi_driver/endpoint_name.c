#include <stdlib.h>

//#define CM_DEBUG 1

#include <stdio.h>
#include <string.h>

#include <CoreServices/CoreServices.h>
#include <CoreMIDI/MIDIServices.h>
#include <CoreAudio/HostTime.h>

//
// Code taken from http://developer.apple.com/qa/qa2004/qa1374.html
//////////////////////////////////////
// Obtain the name of an endpoint without regard for whether it has connections.
// The result should be released by the caller.
CFStringRef EndpointName(MIDIEndpointRef endpoint, bool isExternal)
{
  CFMutableStringRef result = CFStringCreateMutable(NULL, 0);
  CFStringRef str;
  
  // begin with the endpoint's name
  str = NULL;
  MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &str);
  if (str != NULL) {
    CFStringAppend(result, str);
    CFRelease(str);
  }
  
  MIDIEntityRef entity = NULL;
  MIDIEndpointGetEntity(endpoint, &entity);
  if (entity == NULL)
    // probably virtual
    return result;
  
  if (CFStringGetLength(result) == 0) {
    // endpoint name has zero length -- try the entity
    str = NULL;
    MIDIObjectGetStringProperty(entity, kMIDIPropertyName, &str);
    if (str != NULL) {
      CFStringAppend(result, str);
      CFRelease(str);
    }
  }
  // now consider the device's name
  MIDIDeviceRef device = NULL;
  MIDIEntityGetDevice(entity, &device);
  if (device == NULL)
    return result;
  
  str = NULL;
  MIDIObjectGetStringProperty(device, kMIDIPropertyName, &str);
  if (CFStringGetLength(result) == 0) {
      CFRelease(result);
      return str;
  }
  if (str != NULL) {
    // if an external device has only one entity, throw away
    // the endpoint name and just use the device name
    if (isExternal && MIDIDeviceGetNumberOfEntities(device) < 2) {
      CFRelease(result);
      return str;
    } else {
      if (CFStringGetLength(str) == 0) {
        CFRelease(str);
        return result;
      }
      // does the entity name already start with the device name?
      // (some drivers do this though they shouldn't)
      // if so, do not prepend
        if (CFStringCompareWithOptions( result, /* endpoint name */
             str /* device name */,
             CFRangeMake(0, CFStringGetLength(str)), 0) != kCFCompareEqualTo) {
        // prepend the device name to the entity name
        if (CFStringGetLength(result) > 0)
          CFStringInsert(result, 0, CFSTR(" "));
        CFStringInsert(result, 0, str);
      }
      CFRelease(str);
    }
  }
  return result;
}


// Obtain the name of an endpoint, following connections.
// The result should be released by the caller.
static CFStringRef ConnectedEndpointName(MIDIEndpointRef endpoint)
{
  CFMutableStringRef result = CFStringCreateMutable(NULL, 0);
  CFStringRef str;
  OSStatus err;
  int i;
  
  // Does the endpoint have connections?
  CFDataRef connections = NULL;
  int nConnected = 0;
  bool anyStrings = false;
  err = MIDIObjectGetDataProperty(endpoint, kMIDIPropertyConnectionUniqueID, &connections);
  if (connections != NULL) {
    // It has connections, follow them
    // Concatenate the names of all connected devices
    nConnected = CFDataGetLength(connections) / sizeof(MIDIUniqueID);
    if (nConnected) {
      const SInt32 *pid = (const SInt32 *)(CFDataGetBytePtr(connections));
      for (i = 0; i < nConnected; ++i, ++pid) {
        MIDIUniqueID id = EndianS32_BtoN(*pid);
        MIDIObjectRef connObject;
        MIDIObjectType connObjectType;
        err = MIDIObjectFindByUniqueID(id, &connObject, &connObjectType);
        if (err == noErr) {
          if (connObjectType == kMIDIObjectType_ExternalSource  ||
              connObjectType == kMIDIObjectType_ExternalDestination) {
            // Connected to an external device's endpoint (10.3 and later).
            str = EndpointName((MIDIEndpointRef)(connObject), true);
          } else {
            // Connected to an external device (10.2) (or something else, catch-all)
            str = NULL;
            MIDIObjectGetStringProperty(connObject, kMIDIPropertyName, &str);
          }
          if (str != NULL) {
            if (anyStrings)
              CFStringAppend(result, CFSTR(", "));
            else anyStrings = true;
            CFStringAppend(result, str);
            CFRelease(str);
          }
        }
      }
    }
    CFRelease(connections);
  }
  if (anyStrings)
    return result;
  
  // Here, either the endpoint had no connections, or we failed to obtain names for any of them.
  return EndpointName(endpoint, false);
}

char* cm_get_full_endpoint_name(MIDIEndpointRef endpoint)
{
#ifdef OLDCODE
    MIDIEntityRef entity;
    MIDIDeviceRef device;

    CFStringRef endpointName = NULL;
    CFStringRef deviceName = NULL;
#endif
    CFStringRef fullName = NULL;
    CFStringEncoding defaultEncoding;
    char* newName;

    /* get the default string encoding */
    defaultEncoding = CFStringGetSystemEncoding();

    fullName = ConnectedEndpointName(endpoint);
    
#ifdef OLDCODE
    /* get the entity and device info */
    MIDIEndpointGetEntity(endpoint, &entity);
    MIDIEntityGetDevice(entity, &device);

    /* create the nicely formated name */
    MIDIObjectGetStringProperty(endpoint, kMIDIPropertyName, &endpointName);
    MIDIObjectGetStringProperty(device, kMIDIPropertyName, &deviceName);
    if (deviceName != NULL) {
        fullName = CFStringCreateWithFormat(NULL, NULL, CFSTR("%@: %@"),
                                            deviceName, endpointName);
    } else {
        fullName = endpointName;
    }
#endif    
    /* copy the string into our buffer */
    newName = (char *) malloc(CFStringGetLength(fullName) + 1);
    CFStringGetCString(fullName, newName, CFStringGetLength(fullName) + 1,
                       defaultEncoding);

    /* clean up */
#ifdef OLDCODE
    if (endpointName) CFRelease(endpointName);
    if (deviceName) CFRelease(deviceName);
#endif
    if (fullName) CFRelease(fullName);
    return newName;
}
