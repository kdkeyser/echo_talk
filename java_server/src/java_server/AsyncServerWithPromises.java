import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.AsynchronousServerSocketChannel;
import java.nio.channels.AsynchronousSocketChannel;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ExecutionException;

public class AsyncServerWithPromises {

    public static void main(String[] args) {
        try (final AsynchronousServerSocketChannel listener
                = AsynchronousServerSocketChannel.open()) {

            listener.setOption(StandardSocketOptions.SO_REUSEADDR, true);
            listener.bind(new InetSocketAddress("localhost", 6000));

            while (true) {

                ByteBuffer buffer = ByteBuffer.allocateDirect(1024);

                CompletableFuture<AsynchronousSocketChannel> p
                        = new CompletableFuture<AsynchronousSocketChannel>();

                p.complete(accept(listener));

                p.thenApplyAsync((connection) -> {
                    try {
                        connection.read(buffer).get();
                        return connection;
                    } catch (InterruptedException | ExecutionException e) {
                        throw new CompletionException(e);
                    }
                }).thenAcceptAsync((connection) -> {
                    try {
                        buffer.flip();
                        connection.write(buffer).get();
                        connection.close();
                    } catch (InterruptedException | ExecutionException | IOException e) {
                        throw new CompletionException(e);
                    }
                }).exceptionally(t -> {
                    t.printStackTrace();
                    return null;
                });
            }

        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    private static AsynchronousSocketChannel accept(AsynchronousServerSocketChannel assc) {
        try {
            return assc.accept().get();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

}
