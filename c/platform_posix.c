#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include "platform.h"

struct platform_mutex {
    pthread_mutex_t mutex;
};

struct platform_worker {
    worker_t worker;
    struct worker_data *data;
};

static void *platform_worker(void *p)
{
    struct platform_worker *worker = p;
    worker->worker(worker->data);
    free(p);
    return NULL;
}

void platform_thread(worker_t worker, struct worker_data *data)
{
    struct platform_worker *p = malloc(sizeof(*p));
    *p = (struct platform_worker){worker, data};
    pthread_t thread;
    pthread_create(&thread, NULL, platform_worker, p);
}

platform_mutex_t platform_mutex_create(void)
{
    platform_mutex_t mutex = malloc(sizeof(*mutex));
    pthread_mutex_init(&mutex->mutex, NULL);
    return mutex;
}

void platform_mutex_free(platform_mutex_t mutex)
{
    pthread_mutex_destroy(&mutex->mutex);
    free(mutex);
}

void platform_mutex_lock(platform_mutex_t mutex)
{
    pthread_mutex_lock(&mutex->mutex);
}

void platform_mutex_unlock(platform_mutex_t mutex)
{
    pthread_mutex_unlock(&mutex->mutex);
}

int platform_numcores(void)
{
    return sysconf(_SC_NPROCESSORS_ONLN);
}

void platform_sleep(int seconds)
{
    sleep(seconds);
}
