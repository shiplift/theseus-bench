
try:
    import __pypy__
    import gc
    def current_gc_time():
        return gc.get_stats().total_gc_time
except:
    def current_gc_time():
        return 0
