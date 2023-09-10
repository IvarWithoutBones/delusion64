define view-fb
    monitor dump-fb /tmp/delusion64-fb.bin
    shell convert -depth 8 -size 640x480 rgba:/tmp/delusion64-fb.bin /tmp/delusion64-fb.png
    echo wrote converted image to "/tmp/delusion64-fb.png"\n
    shell feh --auto-zoom --fullscreen --image-bg black /tmp/delusion64-fb.png
end
document view-fb
Converts the current VI framebuffer to an image, and displays it using feh.
end

set remotetimeout 9999
echo connecting to localhost:9001\n
target remote localhost:9001
