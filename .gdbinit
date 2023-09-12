define view-fb
    monitor dump-fb /tmp/delusion64-fb.bin
    shell convert -depth 8 -size 640x480 rgba:/tmp/delusion64-fb.bin /tmp/delusion64-fb.png
    if $_shell_exitcode == 0
        echo wrote converted image to "/tmp/delusion64-fb.png"\n

        shell test "$(uname -s)" = "Linux"
        if $_shell_exitcode == 0
            shell feh --auto-zoom /tmp/delusion64-fb.png
        else
            shell test "$(uname -s)" = "Darwin"
            if $_shell_exitcode == 0
                shell open /tmp/delusion64-fb.png
            else
                shell xdg-open /tmp/delusion64-fb.png
            end
        end
    else
        echo failed to convert framebuffer\n
    end
end
document view-fb
Converts the current VI framebuffer to an image, and displays it.
end

set remotetimeout 9999
echo connecting to localhost:9001\n
target remote localhost:9001
