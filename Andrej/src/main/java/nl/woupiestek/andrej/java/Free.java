package nl.woupiestek.andrej.java;

import java.util.function.Consumer;
import java.util.function.Function;

public abstract class Free<T> {

    public final <U> Free<U> flatMap(Function<T, Free<U>> f) {
        return fold(new Folder<Free<U>>() {
            @Override
            public <Request, Response> Free<U> onRequest(Request request, Function<Response, Free<U>> function) {
                return new Bind<Request, Response, U>(request) {
                    @Override
                    public Free<U> getNext(Response s) {
                        return function.apply(s);
                    }
                };
            }
        }, f);
    }

    public final <U> Free<U> map(Function<T, U> f) {
        return flatMap(p -> new Unit<U>(f.apply(p)));
    }

    @FunctionalInterface
    public interface Eacher {
        <Request, Response> void onRequest(Request request, Consumer<Response> consumer);
    }

    public abstract void foreach(Eacher onBind, Consumer<T> onUnit);

    @FunctionalInterface
    public interface Folder<Target> {
        <Request, Response> Target onRequest(Request request, Function<Response, Target> function);
    }

    public abstract <U> U fold(Folder<U> onBind, Function<T, U> onUnit);

    public final boolean hasNext() {
        return fold(new Folder<Boolean>() {
            @Override
            public <Request, Response> Boolean onRequest(Request request, Function<Response, Boolean> function) {
                return true;
            }
        }, x -> false);
    }

    private static final class Unit<Value> extends Free<Value> {
        public final Value value;

        public Unit(Value value) {
            this.value = value;
        }

        @Override
        public void foreach(Eacher eacher, Consumer<Value> onUnit) {
            onUnit.accept(value);
        }

        @Override
        public <U> U fold(Folder<U> onBind, Function<Value, U> onUnit) {
            return onUnit.apply(value);
        }
    }

    private static abstract class Bind<Request, Response, Value> extends Free<Value> {
        public final Request request;

        Bind(Request request) {
            this.request = request;
        }

        public abstract Free<Value> getNext(Response s);

        @Override
        public final void foreach(Eacher onBind, Consumer<Value> onUnit) {
            onBind.onRequest(request, (Response x) -> getNext(x).foreach(onBind, onUnit));
        }

        @Override
        public final <U> U fold(Folder<U> onBind, Function<Value, U> onUnit) {
            return onBind.onRequest(request, (Response x) -> getNext(x).fold(onBind, onUnit));
        }
    }
}
