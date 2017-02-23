package nl.woupiestek.andrej;

import java.util.function.Function;


public abstract class JFree<M, T> {

    @FunctionalInterface
    interface Folder<M, P> {
        <Q> P fold(M m, Function<Q, P> f);
    }

    public abstract <U> U fold(Folder<M, U> onBind, Function<T, U> onReturn);

    public final <U> JFree<M, U> flatMap(Function<T, JFree<M, U>> f) {
        return fold(Bind::new, f);
    }

    public final <U> JFree<M, U> map(Function<T, U> f) {
        return flatMap(t -> new Return<>(f.apply(t)));
    }

    private static class Return<N, Q> extends JFree<N, Q> {

        private final Q value;

        Return(Q q) {
            value = q;
        }

        @Override
        public <U> U fold(Folder<N, U> onBind, Function<Q, U> onReturn) {
            return onReturn.apply(value);
        }
    }

    public static <M, T> JFree<M, T> unit(T t) {
        return new Return<>(t);
    }

    private static class Bind<M, P, Q> extends JFree<M, Q> {

        private final M effect;

        private final Function<P, JFree<M, Q>> callback;

        Bind(M e, Function<P, JFree<M, Q>> c) {
            effect = e;
            callback = c;
        }

        @Override
        public <U> U fold(Folder<M, U> onBind, Function<Q, U> onReturn) {
            return onBind.fold(effect, (P p) -> callback.apply(p).fold(onBind, onReturn));
        }
    }

    public static <M, T> JFree<M, T> lift(M m) {
        return new Bind<M, T, T>(m, Return::new);
    }

}
