#pragma once
#include <memory>
#include <type_traits>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <cilk/cilk.h>

struct closure;
class spawn_write_dest;

typedef void (*task_fn_t)(std::shared_ptr<closure> args);

struct cont {
private:
    spawn_write_dest* mySN;
public:
    closure *cls;
    void* ret;

    void init(spawn_write_dest* mySN1, closure* cls1) {
        mySN = mySN1;
        cls = cls1;
    }

    spawn_write_dest* getSN() const { return mySN; };
};

class closure { 
public:
    cont k;
    int jc;

    virtual task_fn_t getTask() { return nullptr; };
    closure (cont k): k(k) {}
};

class spawn_write_dest {
public:
    std::vector<closure*> toSpawn;
    int *jc;

    void put_spawn(closure* C) {
        toSpawn.push_back(C);
        (*jc)++;
    }
};

void taskSpawn(task_fn_t fn, std::shared_ptr<closure> cls) {
    fn(cls);
}

template <class C> class spawn_next: public spawn_write_dest {
public:
    std::shared_ptr<C> cls;

    spawn_next(C incls) {
        static_assert(std::is_base_of<closure, C>::value, "spawn next parameter should be derived from closure.");
        C *cls_temp = new C(incls);
        cls_temp->jc = 0;
        jc = &(cls_temp->jc);
        cls = std::shared_ptr<C>(cls_temp);
    }

    ~spawn_next() {
        for (closure *spawnCls : toSpawn) {
            cilk_spawn taskSpawn(spawnCls->getTask(), std::shared_ptr<closure>(spawnCls));
        }
        cilk_sync;
        assert(cls->jc == 0);
        //for (closure *spawnCls : toSpawn) {
        //    delete spawnCls;
        //}
        cilk_spawn taskSpawn(cls->getTask(), cls);
    }
};

template <class C> class spawn {
    public:
    spawn(C cls) {
        static_assert(std::is_base_of<closure, C>::value, "spawn next parameter should be derived from closure.");
        if (cls.k.ret) {
            assert((cls.k).getSN());
            C* newCls = new C(cls);
            ((cls.k).getSN())->put_spawn((closure*)newCls);
        } else {
            printf("unsupported\n");
            exit(1);
        }
    }
};

#define SEND_ARGUMENT(k, n) {*((typeof(n)*)((k).ret)) = (n); ((k).cls)->jc--; return; }
#define SN_BIND(sn, k, field) {assert(sn.cls); (k)->init(&sn,sn.cls.get()); (k)->ret = (void*)&(sn.cls->field);}
#define SN_BIND_EXT(sn, k, ptr) {assert(sn.cls); (k)->init(&sn,sn.cls.get()); (k)->ret = (void*)ptr;}
#define THREAD(fn_name) void fn_name (std::shared_ptr<closure> args)
#define CLOSURE_DEF(name, ...) \
struct name##_closure : public closure { \
    __VA_ARGS__ \
    using closure::closure; \
    task_fn_t getTask() override { \
        return &name; \
    } \
};
#define CONT_DUMMY (cont{})