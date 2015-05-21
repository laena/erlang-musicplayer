#include <erl_driver.h>

#if ERL_DRV_EXTENDED_MAJOR_VERSION == 1
    typedef int  ErlDrvSizeT;
    typedef int  ErlDrvSSizeT;
#endif

#include "bass.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf, 
           int len, char **rbuf, int rlen); 
//static void ready_io(ErlDrvData drv_data, ErlDrvEvent event);

static ErlDrvEntry bass_driver_entry = {
    NULL,           /* init */
    start, 
    stop,
    NULL,           /* output */
    NULL,//ready_io,       /* ready_input */
    NULL,//ready_io,       /* ready_output */ 
    "bassdriver",   /* the name of the driver */
    NULL,           /* finish */
    NULL,           /* handle */
    control, 
    NULL,           /* timeout */
    NULL,           /* outputv */
    NULL,           /* ready_async */
    NULL,           /* flush */
    NULL,           /* call */
    NULL,            /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MINOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION -> CHECK IF REALLY MINOR */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};

typedef struct our_data_t {
    int streamID;
} our_data_t;

static int do_play(const char *s, our_data_t* data);
static int do_init();
static int do_stop(our_data_t* data);
static int do_set_pos(const char* pos, const our_data_t* data);
static char* get_s(const char* buf, int len);

#define DRV_PLAY                'P'
#define DRV_STOP                'S'
#define DRV_SET_POS             'T'
#define DRV_INIT                'I'

DRIVER_INIT(pq_drv)
{
    return &bass_driver_entry;
}

static ErlDrvData start(ErlDrvPort port, char *command)
{ 
    our_data_t* data = driver_alloc(sizeof(our_data_t));
    data->streamID = 0;
    return (ErlDrvData)data;
}

static void stop(ErlDrvData drv_data)
{
    do_stop((our_data_t*)drv_data);
}

static int control(ErlDrvData drv_data, unsigned int command, char *buf,
           int len, char **rbuf, int rlen)
{
    int r;
    char* s = get_s(buf, len);
    //printf("Driver command: %c\n", command);
    our_data_t* data = (our_data_t*)drv_data;
    switch (command) {
        case DRV_INIT:
            r = do_init();
            break;
        case DRV_PLAY:
            r = do_play(s, data);
            break;
        case DRV_STOP:
            r = do_stop(data);
            break;
        case DRV_SET_POS:
            r = do_set_pos(s, data);
            break;
    default:
            r = -1;
            break;
    }
    driver_free(s);
    return r;
}

static int do_init()
{
    return BASS_Init(-1, 44100, BASS_DEVICE_DEFAULT, NULL, NULL);
}

static int do_play(const char *s, our_data_t* data)
{
    data->streamID = BASS_StreamCreateFile(FALSE, s, 0 /*offset*/, 0 /*length*/, BASS_DEVICE_DEFAULT);
    if (data->streamID == 0)
    {
        printf("Bass error. Error code: %d\n", BASS_ErrorGetCode());
    }
    return BASS_ChannelPlay(data->streamID, TRUE);
}

static int do_stop(our_data_t* data)
{
    return BASS_ChannelStop(data->streamID);
}

static int do_set_pos(const char* pos, const our_data_t* data)
{
    //printf("Set Position: %d", atoi(pos));
    double dpos;
    sscanf(pos, "%lf", &dpos);
    return BASS_ChannelSetPosition(data->streamID, BASS_ChannelSeconds2Bytes(data->streamID, dpos), BASS_POS_BYTE);
}

static char* get_s(const char* buf, int len)
{
    char* result;
    if (len < 1 || len > 1000) return NULL;
    result = driver_alloc(len+1);
    memcpy(result, buf, len);
    result[len] = '\0';
    return result;
}