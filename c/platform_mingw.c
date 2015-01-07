#include <windows.h>
#include <process.h>
#include "platform.h"

struct platform_worker {
    worker_t worker;
    struct worker_data *data;
};

static void platform_worker(void *p)
{
    struct platform_worker *worker = p;
    worker->worker(worker->data);
    free(p);
}

void platform_thread(worker_t worker, struct worker_data *data)
{
    struct platform_worker *p = malloc(sizeof(*p));
    *p = (struct platform_worker){worker, data};
    _beginthread(platform_worker, 0, p);
}

struct platform_mutex {
    HANDLE mutex;
};

platform_mutex_t platform_mutex_create(void)
{
    platform_mutex_t mutex = malloc(sizeof(*mutex));
    mutex->mutex = CreateMutex(NULL, FALSE, NULL);
    return mutex;
}

void platform_mutex_free(platform_mutex_t mutex)
{
    CloseHandle(mutex->mutex);
    free(mutex);
}

void platform_mutex_lock(platform_mutex_t mutex)
{
    WaitForSingleObject(mutex->mutex, INFINITE);
}

void platform_mutex_unlock(platform_mutex_t mutex)
{
    ReleaseMutex(mutex->mutex);
}

int platform_numcores(void)
{
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
}

void platform_sleep(int seconds)
{
    Sleep(seconds * 1000);
}
