import React, { useState, useEffect, useRef } from "react";
import Websocket from "react-websocket";
import { useSelector, useDispatch } from "react-redux";
import { ADD_MEETINGS } from "../../ducks/meetings";
import { ws } from "../../endpoints";

// this socket is to be used privately inside a meeting?
export function PrivateWs() {
  const wsRef = useRef();
  const [valid, setValid] = useState(false);
  const { refresh, id } = useSelector(({ auth }) => auth);
  const dispatch = useDispatch();

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
    console.log("logging with ", id);
    wsRef.current.sendMessage(id);
  };

  const handleMessage = data => {
    const { meetings } = JSON.parse(data || "{}");

    dispatch({ type: ADD_MEETINGS, meetings });
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
