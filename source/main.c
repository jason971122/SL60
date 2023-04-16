#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define VNODE_CRT    1
#define VNODE_DEL    2
#define VNODE_MIG    3

int g_rec_id = 0;

typedef struct tag_vnode_info_recs
{
    int type;
    int records_id;
    uint64_t request_time;
    uint64_t response_time;
    int results;
} t_vnode_info_recs;

int sl_get_rec_id(void)
{
    return g_rec_id;
}

void api_1(void)
{
    t_vnode_info_recs *vnode_info_recs = NULL;
    vnode_info_recs->type       = VNODE_CRT;
    vnode_info_recs->records_id = sl_get_rec_id()++;
}

int main()
{
    api_1();
    return g_rec_id;
}
