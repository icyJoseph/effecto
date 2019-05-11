import React, { useState, useEffect, useRef } from "react";
import Websocket from "react-websocket";
import { useSelector } from "react-redux";
import { ws } from "../../endpoints";

// this socket is to be used privately inside a meeting?
export function PrivateWs() {
  const wsRef = useRef();
  const [valid, setValid] = useState(false);
  const { refresh, id } = useSelector(({ auth }) => auth);

  useEffect(() => {
    if (refresh) {
      const now = new Date();
      const refreshTime = new Date(refresh);
      if (now - refreshTime < 0) {
        setValid(true);
      }
    }
  }, [refresh]);

  const handleConnection = () => {
    console.log("connected to private server");
    console.log(id);
    wsRef.current.sendMessage(id);
  };

  const handleMessage = data => {
    console.log("message", data);
  };

  return (
    valid && (
      <Websocket
        url={`${ws}/websocketserver`}
        onOpen={handleConnection}
        onMessage={handleMessage}
        ref={wsRef}
      />
    )
  );
}

export default PrivateWs;
