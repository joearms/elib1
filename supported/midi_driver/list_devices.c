#include <CoreMIDI/CoreMIDI.h>    /* interface to MIDI in Macintosh OS X */
#include <unistd.h>               /* for sleep() function                */
#define MESSAGESIZE 3             /* byte count for MIDI note messages   */

// try to list all midi devices

char *cm_get_full_endpoint_name(MIDIEndpointRef);

void print_devices(){
  int i, n;
  char *str;
  MIDIEndpointRef endpoint;

  n=MIDIGetNumberOfDestinations();
  // printf("number of destinations = %d \n", n);
  printf("[");
  for(i=0; i<n;i++){
    endpoint = MIDIGetDestination(i);
    str = cm_get_full_endpoint_name(endpoint); 
    printf("{dest,%d,\"%s\"},\n", i, str);
 
  };
  
  n = MIDIGetNumberOfDevices();
  // printf("number of devices = %d \n", n);
  for(i=0; i<n;i++){
    endpoint = (MIDIEndpointRef) MIDIGetDevice(i);
    str = cm_get_full_endpoint_name(endpoint);
    printf("{device,%d,\"%s\"},\n", i, str);
  };
  printf("done]\n");
  
}

int main(void) {
  print_devices();
}

  
