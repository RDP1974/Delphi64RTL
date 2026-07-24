// 23 July 2026 Roberto Della Pasqua - www.dellapasqua.com
#include <tbb/concurrent_queue.h>
#include <stdbool.h>
#define EXPORT __attribute__((visibility("default")))

extern "C" {

    // Crea una nuova coda
    EXPORT tbb::concurrent_queue<void*>* CreateQueue(){
        return new tbb::concurrent_queue<void*>();
    }

    // Libera la memoria della queue
    EXPORT void FreeQueue(tbb::concurrent_queue<void*>* queue){
        delete queue;
    }

    // Aggiunge un puntatore alla coda
    EXPORT void PushToQueue(tbb::concurrent_queue<void*>* queue, void* ptr){
        if (queue) queue->push(ptr);
    }

    // Estrae un puntatore dalla coda
    EXPORT bool PopFromQueue(tbb::concurrent_queue<void*>* queue, void** outPtr){
        if (queue && outPtr && queue->try_pop(*outPtr)) 
            return true;
        return false;
    }

    // Verifica se la coda è vuota
    EXPORT bool IsQueueEmpty(tbb::concurrent_queue<void*>* queue){
        return queue == nullptr || queue->empty();
    }

}
