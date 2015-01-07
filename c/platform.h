#pragma once

struct worker_data;

typedef struct platform_mutex *platform_mutex_t;
typedef void (*worker_t)(struct worker_data *);

void platform_thread(worker_t, struct worker_data *);
platform_mutex_t platform_mutex_create(void);
void platform_mutex_free(platform_mutex_t);
void platform_mutex_lock(platform_mutex_t);
void platform_mutex_unlock(platform_mutex_t);
int  platform_numcores(void);
void platform_sleep(int);
