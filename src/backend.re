let (>>=) = Lwt.bind;

let run = () => {
  let backlog = 5;
  let sockaddr =
    Unix.ADDR_UNIX(Printf.sprintf("/tmp/mir-%d.sock", Unix.getpid()));
  let sock = Lwt_unix.(socket(PF_UNIX, SOCK_STREAM, 0));
  let () = Lwt_unix.bind(sock, sockaddr);
  let () = Lwt_unix.listen(sock, backlog);

  let rec accept_loop = () =>
    Lwt_unix.accept(sock)
    >>= (
      ((fd, saddr)) => {
        Printf.printf("[backend]: Receiving connection from mirari.\n%!");
        let unix_fd = Lwt_unix.unix_file_descr(fd);
        let msgbuf = String.create(11);
        let (nbread, sockaddr, recvfd) =
          Fd_send_recv.recv_fd(unix_fd, msgbuf, 0, 11, []);
        let () =
          Printf.printf(
            "[backend]: %d bytes read, received fd %d\n%!",
            nbread,
            Fd_send_recv.int_of_fd(recvfd),
          );
        let id =
          OS.Netif.id_of_string(String.trim(String.sub(msgbuf, 0, 10)));
        let devtype =
          if (msgbuf.[10] == 'p') {
            OS.Netif.PCAP;
          } else {
            OS.Netif.ETH;
          };
        OS.Netif.add_vif(id, devtype, recvfd);
        Lwt_unix.(shutdown(fd, SHUTDOWN_ALL)); /* Done, we can shutdown the connection now */
        accept_loop();
      }
    );
  accept_loop();
};
